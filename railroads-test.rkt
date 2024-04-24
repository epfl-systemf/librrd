#lang racket/base
(require racket/port)
(require rackunit "railroads.rkt")

(define (draw-rrd-and-compare rrd filename)
  (let ([drawn (string-append filename ".svg")]
        [reference (string-append filename "-ref.svg")])
    (draw-rrd drawn rrd)
    #;(check-equal? (port->string (open-input-file drawn #:mode 'text))
                  (port->string (open-input-file reference #:mode 'text))
                  "SVG output does not match reference")))

(test-case
    "JSON number spec"
  (draw-rrd-and-compare
   '((choice "-" epsilon)
     (choice "0" ("[nonzero digit]" (choice epsilon
                                            (mu "[digit]" (choice epsilon rec)))))
     (choice epsilon ("." (mu "[digit]" (choice epsilon rec))))
     (choice epsilon ((choice "e" "E") (choice "-" epsilon "+") (mu "[digit]" (choice epsilon rec)))))
   "json-number"))

(draw-rrd
 "json-list.svg"
 '("["
   (choice epsilon
           (mu "[token]"
               (choice epsilon ("," rec))))
   "]"))

(draw-rrd "choice-test.svg"
          '("a" "b" (choice (choice "c" "d") "e" "f") "g"))

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
                  (choice ("b"
                           (choice "c" "d" "e")
                           (mu "[thing]" (choice epsilon ("," "and" rec)))
                           rec)
                          ("[thing]" rec)))))
