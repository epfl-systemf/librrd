#lang racket
(require racket/draw racket/gui/base "angle-brackets.rkt")

(the-font-size 9)

(define (gui)
  (define frame (new frame% [label "angle bracket diagrams"] [width 1200] [height 1000]))
  (define my-canvas%
    (class canvas% (super-new)
      (inherit get-dc refresh-now)
      (field [expr '((+ ("WITH" (+ epsilon "RECURSIVE") (<> - "[common-table-expression]" ",")) epsilon)
                     (<> - "[select-core]" (<> + "UNION" ("UNION" "ALL") "INTERSECT" "EXCEPT"))
                     (+ ("ORDER" "BY" (<> - "[ordering-term]" ",")) epsilon)
                     (+ ("LIMIT" "[expr]" (+ epsilon ("OFFSET" "[expr]") ("," "[expr]"))) epsilon))]
             [diag (diagram expr #f #f)]
             [width 950] [self-padding 10]
             [this-justify-content jc-space-evenly]
             [this-align-items ai-top]
             [this-flex-stacks? #f]
             [this-flex-absorb 0.0]
             [this-arrow-threshold 6])
      (define/public (set-expr value)
        (set-field! expr this value)
        (set-field! diag this (diagram value this-flex-stacks? #f))
        (refresh-now))
      (define/public (set-justify-content value)
        (set-field! this-justify-content this value)
        (refresh-now))
      (define/public (set-align-items value)
        (set-field! this-align-items this value)
        (refresh-now))
      (define/public (set-flex-absorb value)
        (set-field! this-flex-absorb this value)
        (refresh-now))
      (define/public (set-arrow-threshold value)
        (set-field! this-arrow-threshold this value)
        (refresh-now))
      (define/public (set-flex-stacks value)
        (set-field! this-flex-stacks? this value)
        (set-field! diag this (diagram expr this-flex-stacks? #f))
        (refresh-now))

      (define (measure-desc desc m)
        (string-append (~a (string-append desc ":") #:width 19 #:align 'right)
                       (~a (real->decimal-string m 2) #:width 7 #:align 'right)))

      (define/override (on-paint)
        (let ([dc (get-dc)])
          (parameterize ([justify-content this-justify-content]
                         [align-items this-align-items]
                         [flex-absorb this-flex-absorb])
            (for-each
             (λ (render) (for-each (λ (cmd) (apply dynamic-send dc cmd)) render))
             (let ([ll (parameterize ([distribute-fun distribute-linear]) (send diag lay-out width))]
                   [llqd (parameterize ([distribute-fun distribute-extreme]) (send diag lay-out width))]
                   [lg (send diag lay-out-global width 'default 'default 'ltr)])
               (parameterize ([arrow-threshold this-arrow-threshold])
                 (list
                  (send ll render self-padding self-padding)
                  #;(send llqd render self-padding (+ self-padding 250))
                  (list
                   (list 'draw-text (measure-desc "requested width" width)
                         (+ self-padding (get-field physical-width ll) self-padding) self-padding)
                   (list 'draw-text (measure-desc "heuristic height" (get-field physical-height ll))
                         (+ self-padding (get-field physical-width ll) self-padding) (+ self-padding 20)))
                  (parameterize ([the-atom-box-pen (make-pen #:color "red" #:width 1 #:style 'solid)]
                                 [the-strut-pen (make-pen #:color "red" #:width 1 #:style 'solid)])
                    (send lg render self-padding (+ self-padding 500)))
                  (list (list 'draw-text (measure-desc "optimal height" (get-field physical-height lg))
                              (+ self-padding (get-field physical-width lg) self-padding) (+ self-padding 500))))))))))
      (define/override (on-event event)
        (cond
          [(send event dragging?)
           (set-field! width this (- (send event get-x) self-padding))
           (refresh-now)]
          [(send event button-up? 'left)
           (let* ([target-width (- (send event get-x) self-padding)]
                  [steps 100]
                  [width-incr (exact->inexact (/ (- target-width width) steps))])
             (let loop ([width width] [step steps])
               (when (>= step 0)
                 (set-field! width this width)
                 (refresh-now)
                 (new timer% [notify-callback (λ _ (loop (+ width width-incr) (- step 1)))]
                      [interval 10] [just-once? #t]))))]))))

  (define p1 (new horizontal-pane% [parent frame] [spacing 10]))
  (define canvas (new my-canvas% [parent p1] [min-width 1000] #;[style '(no-autoclear)]))
  (define p2 (new vertical-pane% [parent p1] [spacing 40] [vert-margin 30] [horiz-margin 10]))

  (define p3 (new vertical-pane% [parent p2] [spacing 15] [vert-margin 20]))
  (define expr-textbox (new text-field% [label "angle brackets expression"] [parent p3]
                            [style '(multiple vertical-label)]
                            [init-value #<<>>#
((+ ("WITH"
     (+ epsilon "RECURSIVE")
     (<> - "[common-table-expression]" ","))
    epsilon)
 (<> - "[select-core]"
       (<> + "UNION"
             ("UNION" "ALL")
             "INTERSECT"
             "EXCEPT"))
 (+ ("ORDER" "BY" (<> - "[ordering-term]" ","))
    epsilon)
 (+ ("LIMIT"
     "[expr]"
     (+ epsilon
        ("OFFSET" "[expr]")
        ("," "[expr]")))
    epsilon))
>>#
                                        ]
                            [font (make-font #:family 'modern)]))

  (define p4 (new horizontal-pane% [parent p3] [spacing 50]
                  [alignment '(center center)] [stretchable-height #f]))

  (define justify-content-choices
    (list "space-evenly" "space-between" "space-around" "start" "end" "left" "center" "right"))
  (define justify-content-radio
    (new radio-box%
         [label "justify-content"]
         [choices (map car justify-content-choices)]
         [parent p4]
         [style '(vertical vertical-label)]
         [callback
          (λ (self e)
            (send canvas set-justify-content
                  (cdr (list-ref justify-content-choices (send self get-selection)))))]))

  (define align-items-radio
    (new radio-box%
         [label "align-items"]
         [choices (map car align-items-choices)]
         [parent p4]
         [style '(vertical vertical-label)]
         [callback
          (λ (self e)
            (send canvas set-align-items
                  (cdr (list-ref align-items-choices (send self get-selection)))))]))

  (define p5 (new vertical-pane% [parent p4] [spacing 15] [stretchable-width #f]))

  (define flex-stacks-checkbox
    (new check-box% [label "flex-stacks?"] [parent p5] [value #f]
         [callback
          (λ (self e)
            (send canvas set-flex-stacks (send self get-value)))]))

  (define flex-absorb-slider
    (new slider%
         [label "flex-absorb"]
         [parent p5]
         [min-value 0]
         [max-value 100]
         [min-width 200]
         [stretchable-width #f]
         [init-value 0]
         [style '(horizontal vertical-label)]
         [callback
          (λ (self e)
            (send canvas set-flex-absorb (/ (send self get-value) 100)))]))

  (define arrow-threshold-radio
    (new radio-box%
         [label "how many arrows"]
         [choices '("a few" "some" "lots")]
         [selection 1]
         [parent p5]
         [style '(vertical vertical-label)]
         [callback
          (λ (self e)
            (send canvas set-arrow-threshold
                  (case (send self get-selection) [(0) 15] [(1) 6] [(2) 3])))]))

  (define expr-submit
    (new button%
         [label "submit"]
         [parent p3]
         [callback
          (λ (b e)
            (send canvas set-expr (read (open-input-string (send expr-textbox get-value)))))]))

  (send frame show #t))
