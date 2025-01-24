#lang racket
(require racket/draw "angle-brackets.rkt")

(define my-svg-dc
  (new svg-dc% [width 700] [height 2000] [output "trial.svg"] [exists 'truncate]))
(send my-svg-dc start-doc "")
(send my-svg-dc start-page)

(define my-target (make-bitmap 1500 500))
(define my-bitmap-dc
  (new bitmap-dc% [bitmap my-target]))
(send my-bitmap-dc scale 2 2)

(define diag1 (new station% [terminal? #t] [label "hello"]))
(define diag2 (new sequence%
                   [subs (list (new station% [terminal? #f] [label "good"])
                               (new station% [terminal? #f] [label "bye"])
                               (new station% [terminal? #t] [label "!!!"]))]))
(define diag2-5 (new station% [terminal? #f] [label "foreschmack"]))
(define diag3 (new stack% [diag-top diag1] [diag-bot diag2] [polarity '-]))

(define diag4 (new station% [terminal? #t] [label "Vermont"]))
(define diag5 (new station% [terminal? #f] [label "parametricity"]))
(define diag6 (new stack% [diag-top diag4] [diag-bot diag5] [polarity '+]))

(define diag7 (new stack% [diag-top diag6] [diag-bot diag3] [polarity '+] [flex #f]))
;; (define layout7 (parameterize ([justify-content flex-end])
;;                   (send diag7 lay-out 150 '(logical . 1.6) '(logical . 0) 'rtl)))

(define diag8 (new sequence% [subs (list diag1 diag2-5 diag7 diag4 diag5)] [min-gap 0]))
;; (define layout8 (parameterize ([justify-content space-evenly])
;;                   (send diag8 lay-out 420 'top 'bot 'ltr)))


(define opt-distinct
  (new stack% [flex #f]
       [diag-top (new station% [terminal? #t] [label "DISTINCT"])]
       [diag-bot (new epsilon%)]
       [polarity '+]))

(define exprs
  (new stack% [flex #f]
       [diag-top (new station% [terminal? #f] [label "expr"])]
       [diag-bot (new station% [terminal? #t] [label ","])]
       [polarity '-]))

(define ordering-terms
  (new stack% [flex #f]
       [diag-top (new station% [terminal? #f] [label "ordering-term"])]
       [diag-bot (new station% [terminal? #t] [label ","])]
       [polarity '-]))

(define order
  (new sequence% [subs (list (new station% [terminal? #t] [label "ORDER"])
                             (new station% [terminal? #t] [label "BY"])
                             ordering-terms)]))

(define opt-order
  (new stack% [flex #f]
       [diag-top (new epsilon%)]
       [diag-bot order]
       [polarity '+]))

(define opt-star
  (new stack%
       [diag-top (new station% [terminal? #t] [label "*"])]
       [diag-bot (new epsilon%)]
       [polarity '+]))

(define function-arguments-diagram
  (new stack% [flex #f]
       [diag-top (new sequence% [subs (list opt-distinct exprs opt-order)] [flex-absorb 0])]
       [diag-bot opt-star]
       [polarity '+]))

(define function-arguments-layouts
  (let ([max-content (+ (* 2 min-strut-width) (get-field max-content function-arguments-diagram))]
        [min-content (+ (* 2 min-strut-width) (get-field min-content function-arguments-diagram))])
    (map (λ (width)
           (let ([layout (send function-arguments-diagram lay-out width 'default 'default 'ltr)])
             (list width (get-field physical-height layout) layout)))
         (list
          (+ max-content 10)
          max-content
          (/ (+ (* 4 max-content) min-content) 5)
          (/ (+ (* 3 max-content) (* 2 min-content)) 5)
          (/ (+ (* 2 max-content) (* 3 min-content)) 5)
          (/ (+ max-content (* 4 min-content)) 5)
          min-content
          (- min-content 10)))))

(define (function-arguments-render!)
  (apply foldl (λ (width height layout total)
                 (for-each (λ (cmd) (apply dynamic-send my-svg-dc cmd))
                           (send layout render 10 total))
                 (send my-svg-dc draw-text
                       (string-append "requested width: " (~a width))
                       500 total)
                 (send my-svg-dc draw-text
                       (string-append "actual width: " (~a (get-field physical-width layout)))
                       500 (+ total 20))
                 (+ total height 30))
         10
         (apply map list function-arguments-layouts)))

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
