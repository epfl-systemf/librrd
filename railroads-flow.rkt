#lang racket
(require (for-syntax racket/syntax))

(define-syntax (with-destruct-object-as stx)
  (syntax-case stx ()
    [(_ obj (fields ...) body)
     (and (identifier? #'obj) (andmap identifier? (syntax->list #'(fields ...))))
     (let* ([prefixer (lambda (id) (format-id id "~a-~a" #'obj id))]
            [prefixed (map prefixer (syntax->list #'(fields ...)))])
       (with-syntax ([(names ...) prefixed])
         #'(let ([names (get-field fields obj)] ...)
             body)))]))


(define/contract (make-trs leaf? equal? rules)
  (-> (-> any/c any/c) (-> any/c any/c boolean?) (-> any/c any/c) (-> any/c any/c))
  (define (rewrite expr)
    (if (leaf? expr)
        expr
        (let* ([inner-rewritten (if (list? expr) (map rewrite expr) expr)]
               [rewritten (with-handlers ([exn:misc:match? (lambda (e) inner-rewritten)])
                            (rules inner-rewritten))])
          (if (equal? rewritten inner-rewritten)
              rewritten
              (rewrite rewritten)))))
  rewrite)

(define (my-rules expr)
  (match expr
    [(list '+ (and number? lhs) (and number? rhs)) (+ lhs rhs)]
    [(list '* (and number? lhs) (and number? rhs)) (* lhs rhs)]
    [(list '+ 0 e) e]
    [(list '+ e 0) e]
    [(list '* 0 e) 0]
    [(list '* e 0) 0]
    [(list '* 1 e) e]
    [(list '* e 1) e]
    [(list '* a (list '+ b c)) `(+ (* ,a ,b) (* ,a ,c))]
    [(list '* (list '+ b c) a) `(+ (* ,b ,a) (* ,c ,a))]
    [(list '* (list '* a b) c) `(* ,a (* ,b ,c))]
    ; termination-breaking rule
    #;[(list '* a (list '* b c)) `(* (* ,a ,b) ,c)]))

(define my-arith-simplify
  (make-trs (lambda (e) (not (list? e))) equal? my-rules))
(my-arith-simplify '(+ a 0))
(my-arith-simplify '(+ (+ b c) 0))
(my-arith-simplify '(+ (+ b (* 0 c)) 0))
(my-arith-simplify '(* d (* a (+ 1 b))))


(define (syntax-normalization-leaf? expr)
  (and (list? expr) (member (car expr) (list 'term 'nonterm 'rec 'epsilon))))

(define ((rec-free? level) expr)
  (match expr
    [(list* (or 'term 'nonterm 'epsilon) _) #t]
    [(list 'rec l) (not (= l level))]
    [(or (list* (or '+ 'seq) exprs) (list* '<> _ exprs)) (andmap (rec-free? level) exprs)]
    [(list* 'mu expr) ((rec-free? (+ level 1)) expr)]))

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

(define (syntax-normalization-rules expr)
  (match expr
    [(list 'mu exprs ..2) (list 'mu (cons 'seq exprs))]
    [(? string?) (if (and (string-prefix? expr "[") (string-suffix? expr "]"))
                     (list 'nonterm (substring expr 1 (- (string-length expr) 1)))
                     (list 'term expr))]
    ['epsilon '(epsilon)]
    ['rec (list 'rec 0)]
    [(list* (? syntax-normalization-leaf?) _) (cons 'seq expr)]
    ; seq splicing --- we could also not
    [(list* 'seq exprs)
     (cons 'seq (append-map
                 (lambda (e) (if (and (list? e) (equal? (car e) 'seq)) (cdr e) (list e)))
                 exprs))]
    ; (mu (seq exprs ... e)) == (seq (mu (seq exprs ...)) e)
    ; when e does not have a rec
    [(or (list 'mu (list* 'seq exprs)) (list '<> '- (list* 'seq exprs) _))
     (displayln (last exprs))
     (if ((rec-free? 0) (last exprs))
         `(seq (mu (seq ,@(drop-right exprs 1))) ,(last exprs))
         (let* ([partitioned (partition-tail-recursions (cons 'seq exprs) 0)]
                [forward (car partitioned)]
                [backwards (cdr partitioned)])
           (foldr (lambda (bw acc) (list '<> '- acc bw))
                  forward backwards)))]
    [(list '+ expr) expr]
    [(list '+ expr1 expr2) (list '<> '+ expr1 expr2)]
    [(list '+ exprs ..3) (list '<> '+ (first exprs) (cons '+ (rest exprs)))]))

(define syntax-normalizer
  (make-trs syntax-normalization-leaf?
            equal?
            syntax-normalization-rules))




(define (desugar expr)
  (match expr
    [(? string?) (if (and (string-prefix? expr "[") (string-suffix? expr "]"))
                       (list 'nonterm (substring expr 1 (- (string-length expr) 1)))
                       (list 'term expr))]
    ['epsilon '(epsilon)]
    ['rec (list 'rec 0)]
    [(list 'rec _) expr]
    [(list 'mu exprs ..1) (list 'mu (cons 'seq (map desugar exprs)))]
    [(list* '+ exprs) (cons '+ (map desugar exprs))]
    [(list* 'seq exprs) (cons 'seq (map desugar exprs))]
    [(? list?) (cons 'seq (map desugar expr))]
    [_ expr]))


#|
Q: why the special logic for <>-, shouldn't it all be the same as <>+
A: because we've translating from a directional language of mu-rec to the
   adirectional (isotropic?) language of <>
|#

(define (binarize-+-mu expr)
  (match expr
    [(list* '+ expr1 exprs)
     (let ([binarized-expr1 (binarize-+-mu expr1)]
           [binarized-exprs (map binarize-+-mu exprs)])
       (foldl (lambda (expr acc) (list '<> '+ acc expr))
              binarized-expr1 binarized-exprs))]
    [(list* 'seq exprs) (cons 'seq (map binarize-+-mu exprs))]
    [(list 'mu (list* 'seq exprs))
     'pass]
    [(list* 'mu expr)
     'pass]
    [_ expr]))

(binarize-+-mu (desugar '(+ "a" "b" (+ "c1" (seq "c" "2") "c3") "d")))
(binarize-+-mu (desugar '(mu "a" (+ "cc" ("b" rec) "c") (+ ("d" rec) epsilon) "e" "f")))



#|
surface syntax
("a" "b" (+ "c" "d" "e") "f")

desugared
(seq (term "a") (+ (term "c") (term "e")) (term "f"))

diagram
(seq
  (term "a" #:diagram <min-width: X, ...>)
  (+
    (term "c")
    (term "e"))
  (term "f")
  #:diagram <min-width: Y, ...>)

layout
(seq
  (term "a" #:diagram <min-width: X, ...> #:layout<>)
  (+
    (term "c")
    (term "e"))
  (term "f")
  #:diagram <min-width: Y, ...>)

|#

