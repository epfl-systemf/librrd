#lang racket
(require racket/draw "angle-brackets.rkt")

(define my-svg-dc
  (new svg-dc% [width 1200] [height 2300] [output "trial.svg"] [exists 'truncate]))
(send my-svg-dc start-doc "")
(send my-svg-dc start-page)

(define my-target (make-bitmap 1500 500))
(define my-bitmap-dc
  (new bitmap-dc% [bitmap my-target]))
(send my-bitmap-dc scale 2 2)

(define function-arguments-diagram-global
  (diagram '((+ ((+ "DISTINCT" epsilon)
                 (<> - "[expr]" ",")
                 (+ epsilon ("ORDER" "BY" (<> - "[ordering-term]" ","))))
                "*"
                epsilon))))

(define (divide-evenly start end n)
  (for/list ([i (range (+ n 1))])
    (/ (+ (* (- n i) start) (* i end)) n)))

(define function-arguments-layouts
  (let ([max-content (+ #;(* 2 min-strut-width) (get-field max-content function-arguments-diagram-global))]
        [min-content (+ #;(* 2 min-strut-width) (get-field min-content function-arguments-diagram-global))])
    (map (λ (width)
           (let ([layout (send function-arguments-diagram-global lay-out width 'default 'default 'ltr)]
                 [optimal-layout (send function-arguments-diagram-global lay-out-global width 'default 'default 'ltr)])
             (list
              (list width (get-field physical-height layout) layout)
              (list width (get-field physical-height optimal-layout) optimal-layout))))
         (append
          (list (+ max-content 20))
          (divide-evenly max-content min-content 7)
          (list (- min-content 20))))))

(define (function-arguments-render!)
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
                 (send layout render 10 total))
       (for-each (λ (cmd) (apply dynamic-send my-svg-dc cmd))
                 (send optimal-layout render 510 total))
       (send my-svg-dc draw-text
             (string-append "requested width: " (~a width))
             1000 total)
       (send my-svg-dc draw-text
             (string-append "actual width: " (~a (get-field physical-width layout)))
             1000 (+ total 20))
       (send my-svg-dc draw-text
             (string-append "actual height: " (~a height))
             1000 (+ total 40))
       (send my-svg-dc draw-text
             (string-append "optimal height: " (~a optimal-height))
             1000 (+ total 60))
       (+ total height 30)))
   10
   function-arguments-layouts))

(define (show! diag width)
  (send my-bitmap-dc erase)
  (for-each (lambda (cmd) (apply dynamic-send my-bitmap-dc cmd))
            (send (send diag lay-out width 'default 'default 'ltr) render 10 10))
  my-target)

;(println "width")
;(println (get-field physical-width function-arguments-layout))
;(for-each (lambda (cmd) (apply dynamic-send my-bitmap-dc cmd)) (send function-arguments-layout render 10 10))
;my-target

(function-arguments-render!)
(send my-svg-dc end-page)
(send my-svg-dc end-doc)
