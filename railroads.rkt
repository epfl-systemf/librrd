;; '("1" (block (choice "2" ("5" (choose exit nothing))) "3") "4")
;; '("[" (loop "/term" (choose exit ("," (choose exit nothing)))) "]")
#lang racket

(define COUNT 0)
(define railroad-implicit-node%
  (class object% (super-new)
    (init-field [ins '()] [outs '()] [escaped #f])
    (define/public (attach-to! other)
      (set! outs (cons other outs))
      (send other be-attached-from! this))
    (define/public (be-attached-from! other)
      (set! ins (cons other ins)))
    (define/public (detach-from! other)
      (set! outs (remove other outs))
      (send other be-detached-to! this))
    (define/public (be-detached-to! other)
      (set! ins (remove other ins)))
    (field [label (string-append "." (~v COUNT))])
    (set! COUNT (+ COUNT 1))))

(define railroad-node%
  (class railroad-implicit-node% (super-new)
    (inherit-field ins outs label)
    (inherit detach-from! be-detached-to!)
    (init [(lbl label)])
    (init-field style)
    (set! label lbl)
    (define/override (attach-to! other)
      (when (not (empty? outs)) (detach-from! (car outs)))
      (super attach-to! other))
    (define/override (be-attached-from! other)
      (when (not (empty? ins)) (send (car ins) detach-from! this))
      (super be-attached-from! other))))

(define (implicit? n) (and (is-a? n railroad-implicit-node%) (not (is-a? n railroad-node%))))

(define railroad-diagram%
  (class object% (super-new)
    (init-field first-node last-node nodes)
    (define/public (dump!)
      (for-each
       (lambda (n)
         (let ([label (get-field label n)] [outs (get-field outs n)])
           (for-each
            (lambda (nn)
              (println (string-append label "-->" (get-field label nn))))
            outs)))
       nodes))))

(define (railroad-singleton-diagram node)
  (new railroad-diagram% [first-node node] [last-node node] [nodes (list node)]))


(define (parse-railroad rr [stack '()])
  (match rr
    [`(block ,arg)
     (let* ([ln (new railroad-implicit-node%)]
            [parsed (parse-railroad arg (cons (railroad-singleton-diagram ln) stack))])
       (send (get-field last-node parsed) attach-to! ln)
       (new railroad-diagram% [first-node (get-field first-node parsed)] [last-node ln]
            [nodes (set-add (get-field nodes parsed) ln)]))]
    [`(block ,args ...) (parse-railroad `(block ,args) stack)]
    
    [`(loop ,farg ,barg)
     (let* ([fn (new railroad-implicit-node%)]
            [ln (new railroad-implicit-node%)]
            [new-stack (cons (railroad-singleton-diagram ln) stack)]
            [fparsed (parse-railroad farg new-stack)]
            [bparsed (parse-railroad barg new-stack)])
       (send (get-field last-node fparsed) attach-to! (get-field first-node bparsed))
       (send (get-field last-node bparsed) attach-to! fn)
       (send fn attach-to! (get-field first-node fparsed))
       (new railroad-diagram% [first-node fn] [last-node ln]
            [nodes (set-union (get-field nodes fparsed) (get-field nodes bparsed)
                              (list fn ln))]))]
    [`(loop ,farg) (parse-railroad `(loop ,farg nothing) stack)]

    [`(choice ,args ...)
     (let ([parsed (map (lambda (a) (parse-railroad a stack)) args)]
           [fn (new railroad-implicit-node%)]
           [ln (new railroad-implicit-node%)])
       (for-each (lambda (p) (send fn attach-to! (get-field first-node p))) parsed)
       (for-each (lambda (p) (unless (member p stack)
                               (send (get-field last-node p) attach-to! ln))) parsed)
       (new railroad-diagram% [first-node fn] [last-node ln]
            [nodes
             (set-union
              (apply set-union (map (lambda (p) (get-field nodes p)) parsed))
              (list fn ln))]))]    

    [(list args ...)
     (let ([parsed (map (lambda (a) (parse-railroad a stack)) args)])
       (foldl (lambda (d prev)
                (send (get-field last-node prev) attach-to! (get-field first-node d))
                d)
              (car parsed)
              (cdr parsed))
       (new railroad-diagram%
            [first-node (get-field first-node (first parsed))]
            [last-node (get-field last-node (last parsed))]
            [nodes
             (apply set-union (map (lambda (p) (get-field nodes p)) parsed))]))]

    ['nothing (railroad-singleton-diagram (new railroad-implicit-node%))]
    [`(escape ,n) (list-ref stack n)]
    ['escape ; (escape 0)
     (car stack)]
    [label (railroad-singleton-diagram
            (new railroad-node% [label label] [style 'literal]))]))


(define (squash-implicits-helper rd prev cur)
  (for-each
   (lambda (prev-in) (send* prev-in (detach-from! prev) (attach-to! cur)))
   (get-field ins prev))
  (for-each
   (lambda (prev-out)
     (send prev detach-from! prev-out)
     (unless (equal? cur prev-out) (send cur attach-to! prev-out)))
   (get-field outs prev))
  (set-field! nodes rd (remove prev (get-field nodes rd)))
  (when (equal? prev (get-field first-node rd))
    (set-field! first-node rd cur))
  (squash-implicits! rd))

(define (squash-implicits! rd)
  (let ([nodes (filter implicit? (get-field nodes rd))])
    (let ([single-in (findf (lambda (n) (let ([ins (get-field ins n)])
                                          (and (= 1 (length ins)) (implicit? (car ins)))))
                            nodes)])
      (if single-in
          (squash-implicits-helper rd (car (get-field ins single-in)) single-in)
          (let ([single-out
                 (findf (lambda (n) (let ([outs (get-field outs n)])
                                      (and (= 1 (length outs)) (implicit? (car outs)))))
                        nodes)])
            (when single-out
              (squash-implicits-helper rd single-out (car (get-field outs single-out)))))))))
