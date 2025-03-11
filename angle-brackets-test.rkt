#lang racket
(require racket/draw "angle-brackets.rkt")

(define my-svg-dc
  (new svg-dc% [width 2500] [height 3500] [output "trial.svg"] [exists 'truncate]))
(send my-svg-dc start-doc "")
(send my-svg-dc start-page)
(send my-svg-dc set-smoothing 'smoothed)

(define my-target (make-bitmap 1500 500))
(define my-bitmap-dc
  (new bitmap-dc% [bitmap my-target]))
(send my-bitmap-dc scale 2 2)

#;(define function-arguments-diagram-global
  (diagram '((+ ((+ "DISTINCT" epsilon)
                 (<> - "[expr]" ",")
                 (+ epsilon ("ORDER" "BY" (<> - "[ordering-term]" ","))))
                "*"
                epsilon))))

#;(define compound-select
  (diagram '((+ ("WITH" (+ epsilon "RECURSIVE") (<> - "[common-table-expression]" ","))
                epsilon)
             (<> - "[select-core]" (<> + "UNION" ("UNION" "ALL") "INTERSECT" "EXCEPT"))
             (+ ("ORDER" "BY" (<> - "[ordering-term]" ","))
                epsilon)
             (+ ("LIMIT" "[expr]" (+ epsilon ("OFFSET" "[expr]") ("," "[expr]")))
                epsilon))))

(define (divide-evenly start end n)
  (for/list ([i (range (+ n 1))])
    (/ (+ (* (- n i) start) (* i end)) n)))

(define (make-layouts diag)
  (let ([max-content (send diag max-content 'default 'default)]
        [min-content (send diag min-content 'default 'default)])
    (map (λ (width)
           (let ([layout (send diag lay-out width 'default 'default 'ltr)]
                 [optimal-layout (send diag lay-out-global width 'default 'default 'ltr)])
             (list
              (list width (get-field physical-height layout) layout)
              (list width (get-field physical-height optimal-layout) optimal-layout))))
         (append
          #;(list (+ max-content 20))
          (divide-evenly max-content min-content 7)
          #;(list (- min-content 20))))))

(define (measure-desc desc m)
  (string-append (~a (string-append desc ":")
                     #:width 19 #:align 'right)
                 (~a (real->decimal-string m 2)
                     #:width 7 #:align 'right)))

(define (render-layouts! layouts)
  (foldl
   (λ (ls total)
     (let* ([approx (first ls)]
            [optimal (second ls)]
            [width (first approx)]
            [height (second approx)]
            [layout (third approx)]
            [optimal-height (second optimal)]
            [optimal-layout (third optimal)])
       (for-each (λ (cmd) (apply dynamic-send my-svg-dc cmd))
                 (send layout render 250 total))
       (for-each (λ (cmd) (apply dynamic-send my-svg-dc cmd))
                 (send optimal-layout render 1350 total))
       (send my-svg-dc draw-text (measure-desc "requested width" width)
             10 total)
       (send my-svg-dc draw-text (measure-desc "actual width" (get-field physical-width layout))
             10 (+ total 20))
       (send my-svg-dc draw-text (measure-desc "actual height" height)
             10 (+ total 40))
       (send my-svg-dc draw-text (measure-desc "optimal height" optimal-height)
             10 (+ total 60))
       (+ total height 40)))
   10
   layouts))

(define (show! diag width)
  (send my-bitmap-dc erase)
  (for-each (lambda (cmd) (apply dynamic-send my-bitmap-dc cmd))
            (append
             (parameterize ([align-items ai-bottom]
                            [justify-content jc-start])
               (let* ([specs '(vertical vertical ltr)]
                      [l (send/apply diag lay-out width specs)])
                 (displayln (get-field physical-width l))
                 (displayln (send/apply diag min-content specs))
                 (append
                  (send l render 10 10)
                  (list `(draw-line 10 100 ,(+ 10 (get-field physical-width l)) 100)))))
             #;(send
              (parameterize (#;[distribute-fun distribute-extreme])
                (send diag lay-out-global width 'default 'default 'ltr))
              render 10 200)))
  my-target)

(define my-diagram
  (diagram
   '(<> - "hello" ("," "and"))
   ;'("," "and")
   ;'(+ epsilon ("a" (+ epsilon "a2")) "b")
   #t #f))

(show! my-diagram 200)

;; (render-layouts! (make-layouts compound-select))
(send my-svg-dc end-page)
(send my-svg-dc end-doc)
