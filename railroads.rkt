#lang racket

;; syntax:
;; "a"                  alias for (term "a")
;; (term "a")           terminal
;; "[a]"                alias for (nonterm "a")
;; (nonterm "a")        named nonterminal
;; (seq A ...)          sequence
;; (A ...)              alias for (seq A ...)
;; (choice A ...)       choice
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
    [(list* 'choice exprs) (cons 'choice (map normalize-syntax exprs))]
    [(? list?) (cons 'seq (map normalize-syntax expr))]
    [_ expr]))

(define (count-holes expr level)
  ;; Counts the number of holes at the given de Bruijn level in expr.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) '(epsilon)) 0]
    [(list 'hole l _) (if (= l level) 1 0)]
    [(list 'mu sub) (count-holes sub (+ 1 level))]
    [(or (list* 'choice subs) (list* 'seq subs))
     (apply + (map (lambda (s) (count-holes s level)) subs))]))

(define (shift-holes n expr level)
  ;; Increments all hole numbers at the given de Bruijn level by n in expr.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) '(epsilon)) expr]
    [(list 'hole l k) (if (= l level) (list 'hole l (+ n k)) expr)]
    [(list 'mu sub) (list 'mu (shift-holes n sub (+ 1 level)))]
    [(list* 'choice subs) (cons 'choice (map (lambda (s) (shift-holes n s level)) subs))]
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
    [(list* 'choice subs)
     (let* ([subparts (map (lambda (e) (partition-tail-recursions e level)) subs)]
            [subctxs (map (lambda (p) (car p)) subparts)]
            [subrecs (map (lambda (p) (cdr p)) subparts)])
       (if
        (andmap (lambda (ctx) (eq? (car ctx) 'hole)) subctxs)   ; each subrecs has length 1
        (cons (list 'hole level 0)
              (list (cons 'choice (apply append (reverse subrecs)))))
        (cons (cons 'choice
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
    [(list* 'choice subs) (cons 'choice (map logical-representation-normalized subs))]
    [(list* 'seq subs) (cons 'seq (map logical-representation-normalized subs))]))

(define logical-representation
  ;; Turns the input syntax of an RRD into its logical representation.
  (compose logical-representation-normalized normalize-syntax))


;;; PHASE 2

(struct span (top center bot) #:transparent)
(define (get-span repr)
  (if (eq? (car repr) 'with-top-at) (get-span (caddr repr)) (cadr repr)))
(define linear-top (compose span-top get-span))
(define linear-center (compose span-center get-span))
(define linear-bot (compose span-bot get-span))
(define (linear-height repr)
  (+ (- (linear-bot repr) (linear-top repr)) 1))

(define (stacked reprs pretop)
  (foldl        ; reverses order of choices!
   (lambda (repr acc)
     (let* ([last-bot (car acc)]
            [stacked-so-far (cdr acc)]
            [stacked-c (list 'with-top-at (+ 1 last-bot) repr)])
       (cons (+ last-bot (linear-height repr)) (cons stacked-c stacked-so-far))))
   (cons pretop '())
   reprs))

(define (annotate-lines-shifts logical)
  (match logical
    [(list* (and head (or 'term 'nonterm 'rec 'epsilon)) rests)
     (list* head (span 0 0 0) rests)]
    [(list* 'hole rests)   ; bot := -1 means zero height
     (list* 'hole (span 0 0 -1) rests)]
    [(list* 'choice subs)
     (let* ([choices (map annotate-lines-shifts subs)]
            [height (apply + (map linear-height choices))]
            [half-height (quotient (+ height 1) 2)]
            [stacked-choices (stacked choices (- half-height))])
       (list*
        'choice
        (span (+ 1 (- half-height)) 0 (car stacked-choices))
        (reverse (cdr stacked-choices))))]
    [(list* 'seq subs)
     (let ([annotated (map annotate-lines-shifts subs)])
       (list* 'seq
              (span (apply min (map linear-top annotated))
                    0
                    (apply max (map linear-bot annotated)))
              annotated))]
    [(list* 'mu fsub bsubs)
     (let* ([annotated-fsub (annotate-lines-shifts fsub)]
            [stacked-bsubs (stacked (map annotate-lines-shifts bsubs)
                                    (linear-bot annotated-fsub))])
       (list*
        'mu
        (span (linear-top annotated-fsub) 0 (car stacked-bsubs))
        annotated-fsub
        (reverse (cdr stacked-bsubs))))]))

(define (shifted-span a-span shift)
  (span (+ shift (span-top a-span))
        (+ shift (span-center a-span))
        (+ shift (span-bot a-span))))

(define (resolve-shifts annotated cur-shift)
  (match annotated
    [(list 'with-top-at top sub) (resolve-shifts sub (+ cur-shift (- top (linear-top sub))))]
    [(list* (and head (or 'term 'nonterm 'rec 'epsilon 'hole)) span rests)
     (list* head (shifted-span span cur-shift) rests)]
    [(list* (and head (or 'seq 'choice 'mu)) span subs)
     (list* head
            (shifted-span span cur-shift)
            (map (lambda (r) (resolve-shifts r cur-shift)) subs))]))

(define (linear-physical-representation logical)
  (resolve-shifts (annotate-lines-shifts logical) 0))


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
                    "neighbors" neighbors "other-neighbors" other-neighbors)])]
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

(define-syntax-rule (cons!-and-return expr l)
  (let ([val expr]) (set! l (cons val l)) val))

(define (reverse-expr linear)
  (match linear
    [(list* (or 'term 'nonterm 'rec 'hole 'epsilon) rests) linear]
    [(list* 'seq span subs)
     (list* 'seq span (reverse (map reverse-expr subs)))]
    [(list* (and head (or 'choice 'mu)) span subs)
     (list* head span (map reverse-expr subs))]))

(define (reverse-connections! start)
  (let loop ([agenda (list start)] [seen '()])
    (unless (empty? agenda)
      (let ([node (car agenda)] [news '()])
        (if (memq node seen)
            (loop (cdr agenda) seen)
            (begin
              (set-field! neighbors node
                          (vector-map
                           (lambda (n)
                             (if (eq? n #f) n
                                 (begin
                                   (set! news (cons (second n) news))
                                   (list (not (first n)) (second n)))))
                           (get-field neighbors node)))
              (loop
               (append
                news
                (cdr agenda))
               (cons node seen))))))))

(define (grid-physical-representation linear)
  (let ([all-nodes '()])
    (define (helper linear col recursions)
      (match linear
        [(list (and head (or 'term 'nonterm 'rec)) span label)
         (let ([rrd (cons!-and-return
                     (new rrd-station%
                          [terminal? (eq? head 'term)]
                          [label (if (eq? head 'rec) (string-append "rec " (~a label)) label)]
                          [row (span-center span)] [col col])
                     all-nodes)])
           (list rrd rrd))]
        [(list 'epsilon span)
         (let ([rrd (cons!-and-return
                     (new rrd-junction% [row (span-center span)] [col col])
                     all-nodes)])
           (list rrd rrd))]
        [(list* 'seq span subs)
         (let* ([noneps-subs (filter-not (lambda (s) (eq? (car s) 'epsilon)) subs)]
                [subgrid (helper (car noneps-subs) col recursions)])
           (list
            (first subgrid)
            (foldl
             (lambda (s end)
               (let ([sgrid (helper s (+ 1 (get-field col end)) recursions)])
                 (send end connect-to! (first sgrid))
                 (second sgrid)))
             (second subgrid)
             (cdr noneps-subs))))]
        [(list* 'choice span subs)
         (let-values ([(holes nonholes) (partition (lambda (s) (eq? (car s) 'hole)) subs)])
           (let* ([recs (map
                         (lambda (h) (let ([level (caddr h)] [index (cadddr h)])
                                       (list-ref (list-ref recursions level) index)))
                         holes)]
                  [max-rec-end (if (empty? recs) (- col 1)
                                   (apply max (map (lambda (r) (get-field col (second r))) recs)))]
                  [start (if (> col max-rec-end) col (+ 1 max-rec-end))]
                  [nonhole-grids (map (lambda (nh) (helper nh (+ 1 start) recursions)) nonholes)]
                  [end
                   (+ 1 (apply max (map (lambda (nh) (get-field col (second nh))) nonhole-grids)))]
                  [split (cons!-and-return
                          (new rrd-junction% [row (span-center span)] [col start])
                          all-nodes)])
             (let-values
                 ([(lowest-split join)
                   (cond
                     [(andmap (lambda (s) (eq? (car s) 'epsilon)) nonholes)
                      ; the corresponding nonhole-grids do not have connections and are hence not drawn
                      (values split split)]
                     [(and (= 1 (length nonholes))
                           (= (span-center span) (linear-center (first nonholes))))
                      (let ([center-grid (first nonhole-grids)])
                        (send split connect-to! (first center-grid))
                        (values split (second center-grid)))]
                     [else
                      (let* ([join (cons!-and-return
                                    (new rrd-junction% [row (span-center span)] [col end])
                                    all-nodes)]
                             [split-join (list split join)]
                             [nonhole-junctions-f
                              (lambda (r prev)
                                (let ([nhs (cons!-and-return
                                            (new rrd-junction% [row (get-field row (first r))] [col start])
                                            all-nodes)]
                                      ; (second r) should be on same row as (first r) though
                                      [nhj (cons!-and-return
                                            (new rrd-junction% [row (get-field row (second r))] [col end])
                                            all-nodes)])
                                  (send (first prev) connect-to! nhs)
                                  (send nhs connect-to! (first r))
                                  (send (second r) connect-to! nhj)
                                  (send nhj connect-to! (second prev))
                                  (list nhs nhj)))]
                             [nonhole-junctions-up ; unused in body, named for convenience
                              (foldr nonhole-junctions-f split-join
                                     (filter
                                      (lambda (nhg) (< (get-field row (first nhg)) (span-center span)))
                                      nonhole-grids))]
                             [nonhole-junctions-down
                              (foldl nonhole-junctions-f split-join
                                     (filter
                                      (lambda (nhg) (> (get-field row (first nhg)) (span-center span)))
                                      nonhole-grids))]
                             [nonhole-center-maybe ; unused in body, named for convenience
                              (let ([maybe
                                     (filter
                                      (lambda (nhg) (= (get-field row (first nhg)) (span-center span)))
                                      nonhole-grids)])
                                (unless (empty? maybe)
                                  (send split connect-to! (first (first maybe)))
                                  (send (second (first maybe)) connect-to! join)))])
                        (values (first nonhole-junctions-down) join))])])
               (begin
                 (foldl
                  (lambda (r prev)
                    (let ([hj (cons!-and-return
                               (new rrd-junction% [row (get-field row (second r))] [col start])
                               all-nodes)])
                      (send prev connect-to! hj)
                      (send hj connect-to! (second r))
                      hj))
                  lowest-split
                  recs)
                 (list split join)))))]
        [(list* 'mu span fsub bsubs)
         (let* ([join (cons!-and-return
                       (new rrd-junction% [row (span-center span)] [col col])
                       all-nodes)]
                [bsub-grids
                 (reverse
                  (second
                   (foldl
                    (lambda (s prev)
                      (let ([sjoin (cons!-and-return
                                    (new rrd-junction% [row (linear-center s)] [col col])
                                    all-nodes)]
                            ; TODO: but somehow need to be reversed
                            [sgrid (helper s #;(reverse-expr s) (+ 1 col) recursions)])
                        (reverse-connections! (second sgrid))
                        (send (first sgrid) connect-to! sjoin)
                        (send sjoin connect-to! (first prev))
                        (list sjoin (cons sgrid (second prev)))))
                    (list join '())
                    bsubs)))]
                [fsub-grid
                 (helper fsub (+ 1 col) (cons bsub-grids recursions))])
           (send join connect-to! (first fsub-grid))
           (list join (second fsub-grid)))]))
    (let* ([all-start-end (helper linear 0 '())]
           [all-start (first all-start-end)]
           [all-end (second all-start-end)])
      ; dummy junctions for start and end lines
      ; not added to all-nodes!
      (send (new rrd-junction% [row (get-field row all-start)] [col (- (get-field col all-start) 1)])
            connect-to! all-start)
      (send all-end connect-to!
            (new rrd-junction% [row (get-field row all-start)] [col (+ (get-field col all-end) 1)])))
    all-nodes))

(define rrd
  (compose grid-physical-representation linear-physical-representation logical-representation))

(define (draw-rrd output expr)
  (let ([my-rrd (rrd expr)]
        [my-dc (new svg-dc% [width 1000] [height 300] [output output] [exists 'truncate])])
    (send* my-dc (start-doc "drawing…") (start-page))
    (send my-dc set-origin 30 100)
    (let* ([acc-min-widths-by-col
            (cdr
             (foldl
              (lambda (c acc)
                (let ([this-acc
                       (+ (car acc)
                          (apply max (map (lambda (cc) (send (cdr cc) min-width my-dc)) c)))])
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
                (+ (cdr fr-x) (* (- (cdr cl-x) (cdr fr-x)) (- col (car fr-x))))))])
      (for-each (lambda (component)
                  (send component draw! my-dc col-width-f (lambda (r) (* 30 r)))) my-rrd))
    (send* my-dc (end-page) (end-doc))))

(draw-rrd "json-list.svg"
 '("["
   (choice epsilon
           (mu "[token]"
               (choice epsilon ("," rec))))
   "]"))

(draw-rrd "choice-test.svg"
  '("a" "b" (choice (choice "c" "d") "e" "f") "g"))

(draw-rrd "json-number.svg"
 '((choice "-" epsilon)
   (choice "0" ("[nonzero digit]" (choice epsilon
                                          (mu "[digit]" (choice epsilon rec)))))
   (choice epsilon ("." (mu "[digit]" (choice epsilon rec))))
   (choice epsilon ((choice "e" "E") (choice "-" epsilon "+") (mu "[digit]" (choice epsilon rec))))))

(draw-rrd "well-parenthesized-mus-1.svg"
 '(mu (choice "[phrase]"
              ((mu "[phrase]" (choice ("and" "[phrase]") ("," rec)))))
      (choice ("and" (choice "[phrase]"
                             ((mu "[phrase]" (choice ("and" "[phrase]") ("," rec))))))
              (";" rec))))

(draw-rrd "crossing-mus-1.svg"
 '(mu "a" (mu "b" (choice ("c" (choice ("d" (choice epsilon ("rabcd" (rec 1))))
                                       ("rbc" rec)
                                       ("rabc" (rec 1))))
                          ("rb" rec)))))

(draw-rrd
 "crossing-mus-2.svg"
 '(mu "a" (mu "b" (choice (rec 1) ("c" (choice epsilon rec))))))

(draw-rrd "well-parenthesized-mus-2.svg"
 '(mu
   (mu
    "a"
    (mu
     (mu "b" (choice epsilon ("rb" rec)))
     "c"
     (choice epsilon ("rbc" rec)))
    (choice epsilon ("rabc" rec)))
   "d"
   (choice epsilon ("rabcd" rec))))

(draw-rrd
 "big-backloop-test.svg"
 '(mu "a" (choice epsilon
                  (choice ("b" (choice "c" "d" "e") (mu "[thing]" (choice epsilon ("," rec))) rec)
                          ("[thing]" rec)))))

