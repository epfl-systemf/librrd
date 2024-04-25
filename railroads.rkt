#lang racket

(provide rrd draw-rrd)

;; syntax:
;; "a"                  alias for (term "a")
;; (term "a")           terminal
;; "[a]"                alias for (nonterm "a")
;; (nonterm "a")        named nonterminal
;; (seq A ...)          sequence
;; (A ...)              alias for (seq A ...)
;; (+ A ...)            choice
;; (mu A)               recursive binder
;; (mu A ...)           alias for (mu (seq A ...))
;; (rec n)              recurse to nth enclosing binder, de Bruijn indexed
;; rec                  alias for (rec 0)
;; (epsilon)            nothing
;; epsilon              alias for (epsilon)

;;; PHASE 1
;;;
;;; Turns input syntax into the "logical representation" of RRDs, of which a key
;;; property is that for every mu (recursive binder) form, tail recursion is
;;; factored out to give a forward component and a list of backward (recursive)
;;; components.
;;;
;;; The first step in this phase is to normalize the input syntax, which is
;;; described in the docstring for normalize-syntax. After that, the logical
;;; representation of an RRD is very similar in form to the input syntax, i.e. a
;;; tree of terminals, choices, etc., except that each (mu A) form is replaced
;;; with (mu F B ...):
;;;
;;;  * F is the forward component, also a logical representation (and so mu
;;;    forms inside it can have this structure recursively). It is guaranteed to
;;;    have no rec forms in tail position. Instead, they are all replaced with
;;;    (hole l k) forms. l is the de Bruijn level, with 0 referring to the
;;;    current mu form; k is an index into the list of backward components at
;;;    that level.
;;;
;;;  * The Bs are backward components, themselves all logical representations.
;;;    Each backward component is locally the largest subdiagram that has a rec
;;;    form with the correct level in tail position. So if a sequence ends in
;;;    tail recursion, the entire sequence will be in a backward component; if
;;;    all branches of a choice end in tail recursion, the entire choice will be
;;;    in a backward component; and if an inner mu ends in tail recursion *at
;;;    the outer level*, i.e. has no inner recursion or termination of its own,
;;;    the entire mu will be in a backward component.
;;;
;;; Note that the logical representation does not concern itself with the layout
;;; of the RRD, its graph structure, or any other "physical" properties. These
;;; are handled by the physical representations in later phases.

(define (normalize-syntax expr)
  ;; Normalizes syntactical convenience forms.
  ;;  * "a"             -> (term "a")
  ;;  * "[a]"           -> (nonterm "a")
  ;;  * (A ...)         -> (seq A ...)
  ;;  * (mu A ...)      -> (mu (seq A ...))
  ;;  * rec             -> (rec 0)
  ;;  * epsilon         -> (epsilon)
  ;;
  ;; All further processing assumes normalized syntax.
  (match expr
    [(? string?) (if (and (string-prefix? expr "[") (string-suffix? expr "]"))
                     (list 'nonterm (substring expr 1 (- (string-length expr) 1)))
                     (list 'term expr))]
    ['epsilon '(epsilon)]
    ['rec (list 'rec 0)]
    [(list 'rec _) expr]
    [(list 'mu exprs ..1) (list 'mu (cons 'seq (map normalize-syntax exprs)))]
    [(list* '+ exprs) (cons '+ (map normalize-syntax exprs))]
    [(? list?) (cons 'seq (map normalize-syntax expr))]
    [_ expr]))

(define (count-holes expr level)
  ;; Counts the number of holes at the given de Bruijn level in expr.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) '(epsilon)) 0]
    [(list 'hole l _) (if (= l level) 1 0)]
    [(list 'mu sub) (count-holes sub (+ 1 level))]
    [(or (list* '+ subs) (list* 'seq subs))
     (apply + (map (lambda (s) (count-holes s level)) subs))]))

(define (shift-holes n expr level)
  ;; Increments all hole numbers at the given de Bruijn level by n in expr.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) '(epsilon)) expr]
    [(list 'hole l k) (if (= l level) (list 'hole l (+ n k)) expr)]
    [(list 'mu sub) (list 'mu (shift-holes n sub (+ 1 level)))]
    [(list* '+ subs) (cons '+ (map (lambda (s) (shift-holes n s level)) subs))]
    [(list* 'seq subs) (cons 'seq (map (lambda (s) (shift-holes n s level)) subs))]))

(define (partition-tail-recursions expr level)
  ;; Partitions the given expr into a context with holes and a list of maximal
  ;; subexpressions ending in tail recursion at the given de Bruijn level.
  ;;
  ;; The top-level call to this function should take, as arguments, the
  ;; immediate subexpression of a mu form as expr, and 0 as level. Inner
  ;; recursions within inner mu forms are not touched. They are handled by a
  ;; separate call to this function from partition-tail-recursions.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'hole _ _) '(epsilon)) (cons expr '())]
    [(list* '+ subs)
     (let* ([subparts (map (lambda (e) (partition-tail-recursions e level)) subs)]
            [subctxs (map (lambda (p) (car p)) subparts)]
            [subrecs (map (lambda (p) (cdr p)) subparts)])
       (if
        (andmap (lambda (ctx) (eq? (car ctx) 'hole)) subctxs)   ; each subrecs has length 1
        (cons (list 'hole level 0)
              (list (cons '+ (apply append (reverse subrecs)))))
        (cons (cons '+
                    (cdr (foldr (lambda (ctx acc)
                                  (cons (+ (car acc) (count-holes ctx level))
                                        (cons (shift-holes (car acc) ctx level) (cdr acc))))
                                (cons 0 '())
                                subctxs)))
              (apply append (reverse subrecs)))))]
    [(list 'mu sub)
     (let* ([subpart (partition-tail-recursions sub (+ 1 level))]
            [subctx (car subpart)] [subrecs (cdr subpart)])
       (if (eq? (car subctx) 'hole)     ; thus there is only one subrec
           (cons (list 'hole level 0) (list (list 'mu (car subrecs))))
           (cons (list 'mu subctx) subrecs)))]
    [(list 'rec n)
     (if (= n level) (cons (list 'hole level 0) '((epsilon))) (cons expr '()))]
    [(list 'seq subs ... lastsub)
     (let* ([subpart (partition-tail-recursions lastsub level)]
            [subctx (car subpart)] [subrecs (cdr subpart)])
       (if (eq? (car subctx) 'hole)     ; thus there is only one subrec
           (cons subctx (list (cons 'seq (append subs (list (car subrecs))))))
           (cons (cons 'seq (append subs (list subctx))) subrecs)))]))

(define (logical-representation-normalized expr)
  ;; Turns an RRD in normalized syntax into its logical representation.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) (list 'hole _ _) '(epsilon)) expr]
    [(list 'mu sub)
     (let ([subfbs (map logical-representation-normalized (partition-tail-recursions sub 0))])
       (list* 'mu subfbs))]
    [(list* '+ subs) (cons '+ (map logical-representation-normalized subs))]
    [(list* 'seq subs) (cons 'seq (map logical-representation-normalized subs))]))

(define logical-representation
  ;; Turns the input syntax of an RRD into its logical representation.
  (compose logical-representation-normalized normalize-syntax))


;;; PHASE 2

(struct span (top center bot left right) #:transparent)
(define (get-span repr)
  (if (eq? (car repr) 'with-top-at) (get-span (caddr repr)) (cadr repr)))
(define linear-top (compose span-top get-span))
(define linear-center (compose span-center get-span))
(define linear-bot (compose span-bot get-span))
(define linear-left (compose span-left get-span))
(define linear-right (compose span-right get-span))
(define (linear-height repr)
  (+ (- (linear-bot repr) (linear-top repr)) 1))

(define (stacked reprs pretop)
  (foldl        ; reverses order of reprs!
   (lambda (repr acc)
     (let* ([last-bot (car acc)]
            [stacked-so-far (cdr acc)]
            [stacked-c (list 'with-top-at (+ 1 last-bot) repr)])
       (cons (+ last-bot (linear-height repr)) (cons stacked-c stacked-so-far))))
   (cons pretop '())
   reprs))

(define (reverse-expr expr)
  (let ([rev (lambda (c) (+ (- c) (linear-right expr) (linear-left expr)))])
    (let loop ([e expr])
      (let* ([e-span (get-span e)]
             [new-span (struct-copy span e-span
                                    [left (rev (span-left e-span))]
                                    [right (rev (span-right e-span))])])
        (match e
          [(list 'with-top-at new-top sub) (list 'with-top-at new-top (loop sub))]
          [(list* (and head (or 'term 'nonterm 'rec 'hole)) _ rests)
           (list* head new-span rests)]
          [(list 'epsilon _)
           (list 'epsilon (struct-copy span new-span
                                       [right (- (span-left new-span) 1)]))]
          [(list* (and head (or 'seq 'mu)) _ subs)
           (list* head new-span (map loop subs))]
          [(list* '+ _ type holes nonholes)
           (list* '+ new-span type holes (map loop nonholes))])))))

(define (annotate-lines-shifts logical left recursions)
  (match logical
    [(list* (and head (or 'term 'nonterm 'rec)) rests)
     (list* head (span 0 0 0 left left) rests)]
    [(list 'epsilon) (list 'epsilon (span 0 0 0 left (- left 1)))]
    [(list 'hole level index)   ; bot := -1 means zero height
     (let ([rec (list-ref (list-ref recursions level) index)])
       (list 'hole (span 0 0 -1 #f (linear-right rec)) level index))]
    [(list* '+ subs)
     (let-values ([(holes nonholes) (partition (lambda (s) (eq? (car s) 'hole)) subs)])
       (let* ([annotated-holes (map (lambda (h) (annotate-lines-shifts h #f recursions)) holes)]
              [hole-rights (map (lambda (h) (+ 1 (linear-right h))) annotated-holes)]
              [my-left (if (empty? holes) left (apply max left hole-rights))]
              [annotated-nonholes
               (map (lambda (nh) (annotate-lines-shifts nh (+ 1 my-left) recursions)) nonholes)]
              [height (apply + (map linear-height annotated-nonholes))]
              [half-height (quotient (+ height 1) 2)]
              [stacked-nonholes (stacked annotated-nonholes (- half-height))])
         (let-values ([(my-right type)
                       (if (= (length annotated-nonholes) 1)
                           (let ([the-nonhole (first (cdr stacked-nonholes))])
                             (if (= (linear-top the-nonhole) (cadr the-nonhole))
                                 (if (eq? (caaddr the-nonhole) 'epsilon)
                                     (values my-left 'epsilon-join)
                                     (values (linear-right the-nonhole) 'sub-join))
                                 (values
                                  (+ 1 (apply max (map linear-right annotated-nonholes)))
                                  'explicit-join)))
                           (values
                            (+ 1 (apply max (map linear-right annotated-nonholes)))
                            'explicit-join))])
           (list*
            '+
            (span (+ 1 (- half-height)) 0 (car stacked-nonholes) my-left my-right)
            ; TODO: document
            type annotated-holes
            (reverse (cdr stacked-nonholes))))))]
    [(list* 'seq subs)
     (let ([annotated
            ((compose reverse second foldl)
             (lambda (s acc)
               (let ([annot (annotate-lines-shifts s (first acc) recursions)])
                 (list (+ 1 (linear-right annot)) (cons annot (second acc)))))
             (list left '())
             subs)])
       (list* 'seq
              (span (apply min (map linear-top annotated))
                    0
                    (apply max (map linear-bot annotated))
                    (linear-left (first annotated))
                    (linear-right (last annotated)))
              annotated))]
    [(list* 'mu fsub bsubs)
     (let* ([annotated-bsubs
             (map (lambda (bs) (annotate-lines-shifts bs (+ 1 left) recursions)) bsubs)]
            [reversed-bsubs (map reverse-expr annotated-bsubs)]
            [annotated-fsub
             (annotate-lines-shifts fsub (+ 1 left) (cons annotated-bsubs recursions))]
            [stacked-bsubs (stacked reversed-bsubs (linear-bot annotated-fsub))])
       (list*
        'mu
        (span (linear-top annotated-fsub) 0 (car stacked-bsubs)
              left (linear-right annotated-fsub))
        annotated-fsub
        (reverse (cdr stacked-bsubs))))]))

(define (shifted-span a-span shift)
  (span (+ shift (span-top a-span))
        (+ shift (span-center a-span))
        (+ shift (span-bot a-span))
        (span-left a-span)
        (span-right a-span)))

(define (resolve-shifts annotated cur-shift)
  (match annotated
    [(list 'with-top-at top sub) (resolve-shifts sub (+ cur-shift (- top (linear-top sub))))]
    [(list* (and head (or 'term 'nonterm 'rec 'epsilon 'hole)) span rests)
     (list* head (shifted-span span cur-shift) rests)]
    [(list* (and head (or 'seq 'mu)) span subs)
     (list* head (shifted-span span cur-shift)
            (map (lambda (r) (resolve-shifts r cur-shift)) subs))]
    [(list* '+ span type holes nonholes)
     (list* '+ (shifted-span span cur-shift) type holes
            (map (lambda (r) (resolve-shifts r cur-shift)) nonholes))]))

(define (linear-physical-representation logical)
  (resolve-shifts (annotate-lines-shifts logical 0 '()) 0))


;;; PHASE 3

(require racket/draw)

(define rrd-node%
  (class object% (super-new)
    (init-field row col)
    ; #(e s w n), each is #f or (in? obj)
    (field [neighbors (make-vector 4 #f)])
    (define/public (connect-to! other)
      (let ([other-row (get-field row other)]
            [other-col (get-field col other)]
            [other-neighbors (get-field neighbors other)])
        (cond
          [(and (= row other-row) (= col other-col))
           (raise-arguments-error
            'connect-to!
            "not both row and col can be equal in connection"
            "row" row "col" col)]
          [(= row other-row)
           (cond
             [(and (< col other-col)
                   (eq? #f (vector-ref neighbors 0))
                   (eq? #f (vector-ref other-neighbors 2)))
              (vector-set! neighbors 0 (list #f other))
              (vector-set! other-neighbors 2 (list #t this))]
             [(and (> col other-col)
                   (eq? #f (vector-ref neighbors 2))
                   (eq? #f (vector-ref other-neighbors 0)))
              (vector-set! neighbors 2 (list #f other))
              (vector-set! other-neighbors 0 (list #t this))]
             [else (raise-arguments-error
                    'connect-to!
                    "horizontal connection already occupied"
                    "neighbors" neighbors "other-neighbors" other-neighbors
                    "row" row "col" col "other-row" other-row "other-col" other-col)])]
          [(= col other-col)
           (cond
             [(and (< row other-row)
                   (eq? #f (vector-ref neighbors 1))
                   (eq? #f (vector-ref other-neighbors 3)))
              (vector-set! neighbors 1 (list #f other))
              (vector-set! other-neighbors 3 (list #t this))]
             [(and (> row other-row)
                   (eq? #f (vector-ref neighbors 3))
                   (eq? #f (vector-ref other-neighbors 1)))
              (vector-set! neighbors 3 (list #f other))
              (vector-set! other-neighbors 1 (list #t this))]
             [else (raise-arguments-error
                    'connect-to!
                    "vertical connection already occupied"
                    "neighbors" neighbors "other-neighbors" other-neighbors
                    "row" row "col" col "other-row" other-row)])])))
    (abstract min-width)
    (define/pubment (draw! a-dc col-to-x row-to-y)
      (let ([idxs-outs
             (filter (lambda (n) (and (not (eq? (cdr n) #f)) (eq? (cadr n) #f)))
                     (map (lambda (i) (cons i (vector-ref neighbors i))) (range 0 4)))]
            [anchor-point
             (lambda (col row anchor)
               (list (col-to-x (+ col (list-ref '(0.5 0 -0.5 0) anchor)))
                     (row-to-y (+ row (list-ref '(0 0.5 0 -0.5) anchor)))))])
        (inner '() draw! a-dc col-to-x row-to-y)
        (for-each
         (lambda (idx-out)
           (let ([neighbor-col (get-field col (caddr idx-out))]
                 [neighbor-row (get-field row (caddr idx-out))]
                 [anchor (car idx-out)]
                 [a-path (new dc-path%)])
             (when (or (> (abs (- neighbor-col col)) 1) (> (abs (- neighbor-row row)) 1))
               (send/apply a-path move-to (anchor-point col row anchor))
               (send/apply a-path line-to
                           (anchor-point neighbor-col neighbor-row (modulo (+ anchor 2) 4)))
               (send a-dc draw-path a-path))))
         idxs-outs)))))

(define rrd-junction%
  (class rrd-node% (super-new)
    (inherit-field row col neighbors)
    (define/augment (draw! a-dc col-to-x row-to-y)
      (let* ([conn-idxs (indexes-where (vector->list neighbors) (lambda (n) (not (eq? n #f))))]
             [conn-neighs (map (lambda (i) (cons i (vector-ref neighbors i))) conn-idxs)]
             [ins (filter (lambda (n) (eq? (cadr n) #t)) conn-neighs)]
             [outs (filter (lambda (n) (eq? (cadr n) #f)) conn-neighs)]
             [in-outs (cartesian-product ins outs)]
             [anchor-points (map (lambda (c r) (list (col-to-x c) (row-to-y r)))
                                 (list (+ col 0.5) col (- col 0.5) col)
                                 (list row (+ row 0.5) row (- row 0.5)))]
             [towards-center (lambda (p ratio) (let ([x (first p)] (y (second p)))
                                                 (list (+ x (* ratio (- (col-to-x col) x)))
                                                       (+ y (* ratio (- (row-to-y row) y))))))])
        (send a-dc set-pen "black" 1 'solid)
        (for-each
         (lambda (io)
           (let ([pti (list-ref anchor-points (first (first io)))]
                 [pto (list-ref anchor-points (first (second io)))]
                 [a-path (new dc-path%)])
             (send/apply a-path move-to pti)
             (send/apply a-path line-to (towards-center pti 0.3))
             (send/apply a-path curve-to (append (towards-center pti 0.6)
                                                 (towards-center pto 0.6)
                                                 (towards-center pto 0.3)))
             (send/apply a-path line-to pto)
             (send a-dc draw-path a-path)))
         in-outs)))
    (define/override (min-width a-dc) 20)))

(define rrd-station%
  (class rrd-node% (super-new)
    (init-field terminal? label)
    (inherit-field row col)
    (field [padding-x 5] [padding-y 2])
    (define/override (connect-to! other)
      (let ([other-row (get-field row other)])
        (if (= row other-row)
            (super connect-to! other)
            (raise-arguments-error 'connect-to!
                                   "stations can only be connected horizontally"
                                   "row" row "other-row" other-row))))
    (define/override (min-width a-dc)
      (let-values ([(width height descent extra) (send a-dc get-text-extent label #f #t)])
        ; *4 to have space outside the box too
        (+ width (* 4 padding-x))))
    (define/augment (draw! a-dc col-to-x row-to-y)
      (let-values ([(width height descent extra) (send a-dc get-text-extent label #f #t)])
        (let* ([text-x (- (col-to-x col) (/ width 2))]
               [text-y (- (row-to-y row) (/ height 2))]
               [box-x (- text-x padding-x)]
               [box-y ( - text-y padding-y)]
               [box-width (+ width (* 2 padding-x))]
               [box-height (+ height (* 2 padding-y))])
          (send a-dc set-pen "black" 1 'solid)
          (send a-dc set-brush "white" 'solid)
          (if terminal?
              (send a-dc draw-rounded-rectangle
                    box-x box-y box-width box-height)
              (send a-dc draw-rectangle
                    box-x box-y box-width box-height))
          (send a-dc draw-text label text-x text-y #t)
          (send a-dc draw-line
                (col-to-x (- col 0.5)) (row-to-y row)
                box-x (row-to-y row))
          (send a-dc draw-line
                (+ box-x box-width) (row-to-y row)
                (col-to-x (+ col 0.5)) (row-to-y row)))))))

(define ((extender junction forward?) sub-grids)
  (let ([connect! (if forward?
                      (lambda (s1 s2) (send s1 connect-to! s2))
                      (lambda (s1 s2) (send s2 connect-to! s1)))]
        [subs (map (if forward? first second) sub-grids)]
        [col (get-field col junction)])
    (remq*
     subs
     (foldl
      (lambda (sub prev-juncs)
        (let ([new-row (get-field row sub)])
          (cond
            [(= new-row (get-field row (car prev-juncs)))
             (connect! (car prev-juncs) sub)
             prev-juncs]
            [(= col (get-field col sub)) ; should only happen for epsilon subs
             (connect! (car prev-juncs) sub)
             (cons sub prev-juncs)]
            [else
             (let ([new-junc (new rrd-junction% [row new-row] [col col])])
               (connect! (car prev-juncs) new-junc)
               (connect! new-junc sub)
               (cons new-junc prev-juncs))])))
      (list junction)
      subs))))

(define (grid-helper linear recursions)
  (match linear
    [(list (and head (or 'term 'nonterm 'rec)) (span _ center _ left _) label)
     (let ([rrd (new rrd-station%
                     [terminal? (eq? head 'term)]
                     [label (if (eq? head 'rec) (string-append "rec " (~a label)) label)]
                     [row center] [col left])])
       (list rrd rrd (list rrd)))]
    [(list 'epsilon (span _ center _ left _))
     (let ([rrd (new rrd-junction% [row center] [col left])])
       (list rrd rrd (list rrd)))]
    [(list* 'seq _ subs)
     (let* ([noneps-subs (filter-not (lambda (s) (eq? (car s) 'epsilon)) subs)]
            [first-sub-grid (grid-helper (first noneps-subs) recursions)])
       (list*
        (first first-sub-grid)
        (foldl
         (lambda (sub prev-end-all)
           (let ([sub-grid (grid-helper sub recursions)])
             (send (first prev-end-all) connect-to! (first sub-grid))
             (list (second sub-grid) (append (third sub-grid) (second prev-end-all)))))
         (rest first-sub-grid)
         (rest noneps-subs))))]
    [(list* '+ (span _ center _ left right) type holes nonholes)
     (let* ([recs
             (map
              (lambda (h)
                (let ([level (caddr h)] [index (cadddr h)])
                  (list-ref (list-ref recursions level) index)))
              holes)]
            [split (new rrd-junction% [row center] [col left])]
            [split-extend! (extender split #t)])
       (if (eq? type 'explicit-join)
           (let* ([join (new rrd-junction% [row center] [col right])]
                  [join-extend! (extender join #f)])
             (let*-values
                 ([(nonholes-above nonholes-not-above)
                   (partition (lambda (nh) (< (linear-center nh) center)) nonholes)]
                  [(nonholes-below nonholes-at)
                   (partition (lambda (nh) (> (linear-center nh) center)) nonholes-not-above)]
                  [(nonhole-grids-above nonhole-grids-at nonhole-grids-below)
                   (apply
                    values
                    (map (lambda (nhs) (map (lambda (nh) (grid-helper nh recursions)) nhs))
                         (list nonholes-above nonholes-at nonholes-below)))]
                  [(grids-below) (sort (append nonhole-grids-below recs)
                                       < #:key (lambda (g) (get-field row (first g))))]
                  [(extended-nodes)
                   (append (apply append (map third nonhole-grids-above))
                           (apply append (map third nonhole-grids-at))
                           (apply append (map third nonhole-grids-below))
                           (split-extend! (reverse nonhole-grids-above))
                           (join-extend! (reverse nonhole-grids-above))
                           (split-extend! grids-below)
                           (join-extend! nonhole-grids-below))])
               (unless (empty? nonhole-grids-at)
                 (if (eq? 'epsilon (car (first nonholes-at)))
                     (send split connect-to! join)
                     (begin
                       (send split connect-to! (first (first nonhole-grids-at)))
                       (send (second (first nonhole-grids-at)) connect-to! join))))
               (list split join (cons split (cons join extended-nodes)))))
           (let ([extended-nodes (split-extend! recs)])
             (if (eq? type 'epsilon-join)
                 (list split split (cons split extended-nodes))
                 ; else type = sub-join, i.e. exactly one, centered nonhole sub
                 (let ([the-sub-grid (grid-helper (first nonholes) recursions)])
                   (send split connect-to! (first the-sub-grid))
                   (list split (second the-sub-grid)
                         (cons split (append extended-nodes (third the-sub-grid)))))))))]
    [(list* 'mu (span _ center _ left _) fsub bsubs)
     (let* ([join (new rrd-junction% [row center] [col left])]
            [join-extend! (extender join #f)]
            [bsub-grids (map (lambda (bs) (grid-helper bs recursions)) bsubs)]
            [fsub-grid (grid-helper fsub (cons bsub-grids recursions))]
            [extended-nodes (join-extend! bsub-grids)])
       (send join connect-to! (first fsub-grid))
       (list join (second fsub-grid)
             (cons join (apply append extended-nodes (third fsub-grid)
                               (map third bsub-grids)))))]))

(define (grid-physical-representation linear)
  (let* ([helper (grid-helper linear '())]
         [all-start (first helper)]
         [all-end (second helper)]
         [all-nodes (third helper)])
    ; dummy junctions for start and end lines
    (send (new rrd-junction%
               [row (get-field row all-start)]
               [col (- (get-field col all-start) 1)])
          connect-to! all-start)
    (send all-end connect-to!
          (new rrd-junction%
               [row (get-field row all-start)]
               [col (+ (get-field col all-end) 1)]))
    all-nodes))

(define rrd (compose grid-physical-representation
                     linear-physical-representation
                     logical-representation))

(define (draw-rrd output expr)
  (let* ([my-linear (linear-physical-representation (logical-representation expr))]
         [my-rrd (grid-physical-representation my-linear)]
         [temp-dc (new svg-dc% [width 1000] [height 1000] [output output] [exists 'truncate])]
         [acc-min-widths-by-col
          (cdr
           (foldl
            (lambda (c acc)
              (let ([this-acc
                     (+ (car acc)
                        (apply max (map (lambda (cc) (send (cdr cc) min-width temp-dc)) c)))])
                `(,this-acc . ((,(caar c) . ,this-acc) . ,(cdr acc)))))
            '(0 (-1 . 0))
            (sort
             (group-by car (map (lambda (c) (cons (get-field col c) c)) my-rrd))
             < #:key caar)))]
         [cols (inclusive-range 0 (caar acc-min-widths-by-col))]
         [col-width-f
          (lambda (raw-col)
            (let* ([col (- raw-col 0.5)]
                   [fr-x (assf (lambda (c) (= c (floor col))) acc-min-widths-by-col)]
                   [cl-x (assf (lambda (c) (= c (ceiling col))) acc-min-widths-by-col)])
              (+ (cdr fr-x) (* (- (cdr cl-x) (cdr fr-x)) (- col (car fr-x))))))]
         [row-height-f (lambda (r) (* 30 (+ r (- (linear-top my-linear)) 0.5)))]
         [my-dc (new svg-dc%
                     [width (cdar acc-min-widths-by-col)]
                     [height (- (row-height-f (+ (linear-bot my-linear) 0.5))
                                (row-height-f (- (linear-top my-linear) 0.5)))]
                     [output output] [exists 'truncate])])
    (send* my-dc (start-doc "drawing…") (start-page))
    (for-each (lambda (component)
                (send component draw! my-dc col-width-f row-height-f)) my-rrd)
    (send* my-dc (end-page) (end-doc))))
