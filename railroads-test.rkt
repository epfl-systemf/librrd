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
   '((+ "-" epsilon)
     (+ "0" ("[nonzero digit]" (+ epsilon
                                  (mu "[digit]" (+ epsilon rec)))))
     (+ epsilon ("." (mu "[digit]" (+ epsilon rec))))
     (+ epsilon ((+ "e" "E") (+ "-" epsilon "+") (mu "[digit]" (+ epsilon rec)))))
   "json-number"))

(draw-rrd
 "json-number-alt.svg"
 '((+ "-" epsilon)
   (+ "0" ("[nonzero digit]" (mu (+ epsilon ("[digit]" rec)))))
     (+ epsilon ("." (mu "[digit]" (+ epsilon rec))))
     (+ epsilon ((+ "e" "E") (+ "-" epsilon "+") (mu "[digit]" (+ epsilon rec))))))

(draw-rrd
 "json-list.svg"
 '("["
   (+ epsilon
      (mu "[token]"
          (+ epsilon ("," rec))))
   "]"))

(draw-rrd "choice-test.svg"
          '("a" "b" (+ (+ "c" "d") "e" "f") "g"))

(draw-rrd "well-parenthesized-mus-1.svg"
          '(mu (+ "[phrase]"
                  ((mu "[phrase]" (+ ("and" "[phrase]") ("," rec)))))
               (+ ("and" (+ "[phrase]"
                            ((mu "[phrase]" (+ ("and" "[phrase]") ("," rec))))))
                  (";" rec))))

(draw-rrd "crossing-mus-1.svg"
          '(mu "a" (mu "b" (+ ("c" (+ ("d" (+ epsilon ("rabcd" (rec 1))))
                                      ("rbc" rec)
                                      ("rabc" (rec 1))))
                              ("rb" rec)))))

(draw-rrd "crossing-mus-3.svg"
          '(mu "a" (mu "b" (+ ("c" (+ ("rbc" rec)
                                      epsilon))
                              ("rb" rec)))
               (+ ("d" (+ epsilon ("rabcd" rec)))
                  ("rabc" rec))))

(draw-rrd
 "crossing-mus-2.svg"
 '(mu "a" (mu "b" (+ (rec 1) ("c" (+ epsilon rec))))))

(draw-rrd "well-parenthesized-mus-2.svg"
          '(mu
            (mu
             "a"
             (mu
              (mu "b" (+ epsilon ("rb" rec)))
              "c"
              (+ epsilon ("rbc" rec)))
             (+ epsilon ("rabc" rec)))
            "d"
            (+ epsilon ("rabcd" rec))))

(draw-rrd
 "big-backloop-test.svg"
 '(mu "a" (+ epsilon
             (+ ("b"
                 (+ "c" "d" "e")
                 (mu "[thing]" (+ epsilon ("," "and" rec)))
                 rec)
                ("[thing]" rec)))))

(draw-rrd
 "lots-of-epsilons.svg"
 '((mu (+ epsilon
          ((mu (+ epsilon ((mu (+ epsilon rec)) rec))) rec)))
   (+ epsilon epsilon epsilon)
   (epsilon epsilon "a")))


(draw-rrd
 "regex-style-without-backloop"
 '((mu (+ epsilon ("[CTE]" "," rec))) "[CTE]"))

(draw-rrd
 "els-1.svg"
 '("a" "[b]" "c"))

(draw-rrd
 "els-2.svg"
 '("a" "[b]" (+ "c" "d" (+ "e" "f" "g"))))

(draw-rrd
 "els-3.svg"
 '(mu "a" "b" (+ epsilon ("," rec))))

(draw-rrd
 "els-4.svg"
 '(mu "a" "b" (+ epsilon ("c" rec) ("d" (+ "e" "f") rec))))

(draw-rrd
 "els-thank-you.svg"
 '("Thank"
   (mu "you" (+ epsilon ("and" rec)))
   "!"))
