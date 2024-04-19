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

(define test0 '("[" (choice epsilon (mu "[token]" (choice epsilon ("," rec)))) "]"))
test0

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
    [(list 'mu exprs ..1) (list 'mu (cons 'seq (map normalize-syntax exprs)))]
    [(list* 'choice exprs) (cons 'choice (map normalize-syntax exprs))]
    [(? list?) (cons 'seq (map normalize-syntax expr))]
    [_ expr]))

(define (count-holes expr level)
  ;; Counts the number of holes at the given de Bruijn level in expr.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) '(epsilon)) 0]
    [(list 'hole level _) 1]
    [(list 'mu sub) (count-holes sub (+ 1 level))]
    [(or (list* 'choice subs) (list* 'seq subs))
     (apply + (map (lambda (s) (count-holes s level)) subs))]))

(define (shift-holes n expr level)
  ;; Increments all hole numbers at the given de Bruijn level by n in expr.
  (match expr
    [(or (list 'term _) (list 'nonterm _) (list 'rec _) '(epsilon)) expr]
    [(list 'hole level k) (list 'hole level (+ n k))]
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
    [(or (list 'term _) (list 'nonterm _) '(epsilon)) (cons expr '())]
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

(define test1 (logical-representation test0))
test1


;;; PHASE 2

(struct span (top center bot width) #:transparent)
(define (get-span repr)
  (if (eq? (car repr) 'with-top-at) (get-span (caddr repr)) (cadr repr)))
(define linear-top (compose span-top get-span))
(define linear-center (compose span-center get-span))
(define linear-bot (compose span-bot get-span))
(define linear-width (compose span-width get-span))
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
     (list* head (span 0 0 0 (if (eq? head 'epsilon) 0 1)) rests)]
    [(list* 'hole rests)   ; bot := -1 means zero height
     (list* 'hole (span 0 0 -1 0) rests)]
    [(list* 'choice subs)
     (let* ([choices (map annotate-lines-shifts subs)]
            [height (apply + (map linear-height choices))]
            [half-height (quotient (+ height 1) 2)]
            [stacked-choices (stacked choices (- half-height))])
       (list*
        'choice
        (span (+ 1 (- half-height)) 0 (car stacked-choices)
              ; +1 for the split, and +1 for the join if two or more non-hole choices
              (+ (if (>= (count (lambda (c) (not (eq? (car c) 'hole))) choices) 2) 2 1)
                 (apply max (map linear-width (cdr stacked-choices)))))
        (reverse (cdr stacked-choices))))]
    [(list* 'seq subs)
     (let ([annotated (map annotate-lines-shifts subs)])
       (list* 'seq
              (span (apply min (map linear-top annotated))
                    0
                    (apply max (map linear-bot annotated))
                    (apply + (map linear-width annotated)))
              annotated))]
    [(list* 'mu fsub bsubs)
     (let* ([annotated-fsub (annotate-lines-shifts fsub)]
            [stacked-bsubs (stacked (map annotate-lines-shifts bsubs)
                                    (linear-bot annotated-fsub))])
       (list*
        'mu
        (span (linear-top annotated-fsub) 0 (car stacked-bsubs)
              ; +1 for the recursive join
              ; TODO: the below is an approximation!
              (+ 1 (apply max (map linear-width (cons annotated-fsub (cdr stacked-bsubs))))))
        annotated-fsub
        ; stacked also reverses bsubs, which we want here
        (cdr stacked-bsubs)))]))

(define (shifted-span a-span shift)
  (span (+ shift (span-top a-span)) (+ shift (span-center a-span))
        (+ shift (span-bot a-span)) (span-width a-span)))

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

(define test2
  (linear-physical-representation test1))
test2


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
                    "neighbors" neighbors "other-neighbors" other-neighbors)])])))
    (abstract to-image)))

(define rrd-junction%
  (class rrd-node% (super-new)
    (inherit-field neighbors)
    (define/override (to-image)
      (let* ([conn-idxs (indexes-where (vector->list neighbors) (lambda (n) (not (eq? n #f))))]
             [conn-neighs (map (lambda (i) (cons i (vector-ref neighbors i))) conn-idxs)]
             [ins (filter (lambda (n) (eq? (cadr n) #t)) conn-neighs)]
             [outs (filter (lambda (n) (eq? (cadr n) #f)) conn-neighs)]
             [in-outs (cartesian-product ins outs)])
        in-outs))))

(define rrd-station%
  (class rrd-node% (super-new)
    (init-field terminal? label)
    (inherit-field row)
    (define/override (connect-to! other)
      (let ([other-row (get-field row other)])
        (if (= row other-row)
            (super connect-to! other)
            (raise-arguments-error 'connect-to!
                                   "stations can only be connected horizontally"
                                   "row" row "other-row" other-row))))
    (define/override (to-image)
      label)))

(define (grid-physical-representation linear col recursions)
  (match linear
    [(list (and head (or 'term 'nonterm 'rec)) span label)
     (let ([rrd (new rrd-station%
                     [terminal? (eq? head 'term)]
                     [label (if (eq? head 'rec) (string-append "rec " (~a label)) label)]
                     [row (span-center span)] [col col])])
       (list rrd rrd))]
    [(list 'epsilon span)
     (let ([rrd (new rrd-junction% [row (span-center span)] [col col])])
       (list rrd rrd))]
    [(list* 'seq span sub subs)
     (let ([subgrid (grid-physical-representation sub col recursions)])
       (list
        (first subgrid)
        (foldl
         (lambda (s end)
           (let ([sgrid (grid-physical-representation s (+ 1 (get-field col end)) recursions)])
             (send end connect-to! (first sgrid))
             (second sgrid)))
         (second subgrid)
         (filter-not (lambda (s) (eq? (car s) 'epsilon)) subs))))]
    [(list* 'choice span subs)
     (let-values ([(holes nonholes) (partition (lambda (s) (eq? (car s) 'hole)) subs)])
       (let* ([recs (map
                     (lambda (h) (let ([level (caddr h)] [index (cadddr h)])
                                   (list-ref (list-ref recursions level) index)))
                     holes)]
              [max-rec-end (if (empty? recs) (- col 1)
                               (apply max (map (lambda (r) (get-field col (second r))) recs)))]
              [start (if (> col max-rec-end) col (+ 1 max-rec-end))]
              [nonhole-grids
               (map
                (lambda (nh) (grid-physical-representation nh (+ 1 start) recursions))
                nonholes)]
              [end
               (+ 1 (apply max (map (lambda (nh) (get-field col (second nh))) nonhole-grids)))]
              [split (new rrd-junction% [row (span-center span)] [col start])])
         (let-values
             ([(lowest-split join)
               (if
                (andmap (lambda (s) (eq? (car s) 'epsilon)) nonholes)
                (values split split)
                (let* ([join (new rrd-junction% [row (span-center span)] [col end])]
                       [split-join (list split join)]
                       [nonhole-junctions-f
                        (lambda (r prev)
                          (let ([nhs (new rrd-junction% [row (get-field row (first r))] [col start])]
                                ; (second r) should be on same row as (first r) though
                                [nhj (new rrd-junction% [row (get-field row (second r))] [col end])])
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
                  (values (first nonhole-junctions-down) join)))])
           (begin
             (foldl
              (lambda (r prev)
                (let ([hj (new rrd-junction% [row (get-field row (second r))] [col start])])
                  (send prev connect-to! hj)
                  (send hj connect-to! (second r))
                  hj))
              lowest-split
              recs)
             (list split join)))))]
    [(list* 'mu span fsub bsubs)
     (let* ([join (new rrd-junction% [row (span-center span)] [col col])]
            [bsub-grids
             (reverse
              (second
               (foldl
                (lambda (s prev)
                  (let ([sjoin (new rrd-junction% [row (linear-center s)] [col col])]
                        ; but somehow need to be reversed
                        [sgrid (grid-physical-representation s (+ 1 col) recursions)])
                    (send (first sgrid) connect-to! sjoin)
                    (send sjoin connect-to! (first prev))
                    (list sjoin (cons sgrid (second prev)))))
                (list join '())
                bsubs)))]
            [fsub-grid
             (grid-physical-representation fsub (+ 1 col) (cons bsub-grids recursions))])
       (send join connect-to! (first fsub-grid))
       (list join (second fsub-grid)))]))

(define (display-rrd root seen)
  (cond
    [(memq root seen) '()]
    [(is-a? root rrd-station%)
     (display (get-field label root))
     (display (get-field row root))
     (display (get-field col root))
     (displayln "")
     (vector-map (lambda (n) (display-rrd (cadr n) (cons root (cons n seen))))
                 (vector-filter-not (lambda (n) (eq? n #f)) (get-field neighbors root)))]
    [(is-a? root rrd-junction%)
     (display "junction ")
     (display (get-field row root))
     (display (get-field col root))
     (displayln "")
     (vector-map (lambda (n) (display-rrd (cadr n) (cons root (cons n seen))))
                 (vector-filter (lambda (n) (and (not (eq? n #f)) (not (eq? (car n) #t))))
                                (get-field neighbors root)))]))

(define test3
  (grid-physical-representation
   (linear-physical-representation
    (logical-representation
     '("a" "b" (choice (choice "c" "d") "e" "f") "g")))
   0
   '()))
(display-rrd (first test3) '())
(display-rrd (first (grid-physical-representation test2 0 '())) '())
