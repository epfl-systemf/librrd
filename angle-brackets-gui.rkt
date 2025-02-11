#lang racket
(require racket/draw racket/gui/base "angle-brackets.rkt")

(define (gui)
  (define frame (new frame% [label "angle bracket diagrams"] [width 1200] [height 1000]))
  (define my-canvas%
    (class canvas% (super-new)
      (inherit get-dc refresh-now)
      (field [expr '((+ ("WITH" (+ epsilon "RECURSIVE") (<> - "[common-table-expression]" ",")) epsilon)
                     (<> - "[select-core]" (<> + "UNION" ("UNION" "ALL") "INTERSECT" "EXCEPT"))
                     (+ ("ORDER" "BY" (<> - "[ordering-term]" ",")) epsilon)
                     (+ ("LIMIT" "[expr]" (+ epsilon ("OFFSET" "[expr]") ("," "[expr]"))) epsilon))]
             [diag (diagram expr)]
             [width 950] [self-padding 10]
             [this-justify-content jc-space-evenly]
             [this-align-items ai-top])
      (define/public (set-expr value)
        (set-field! expr this value)
        (set-field! diag this (diagram value))
        (refresh-now repaint))
      (define/public (set-justify-content value)
        (set-field! this-justify-content this value)
        (refresh-now repaint))
      (define/public (set-align-items value)
        (set-field! this-align-items this value)
        (refresh-now repaint))
      (define (repaint dc)
        (parameterize ([justify-content this-justify-content]
                       [align-items this-align-items])
          (for-each
           (λ (render) (for-each (λ (cmd) (apply dynamic-send dc cmd)) render))
           (list
            (send (send diag lay-out width) render self-padding self-padding)
            (let ([l (send diag lay-out-global width)])
              (parameterize ([the-atom-box-pen (make-pen #:color "red" #:width 1 #:style 'solid)]
                             [the-strut-pen (make-pen #:color "red" #:width 1 #:style 'solid)])
                (send l render self-padding (+ self-padding 500))))))))
      (define/override (on-paint) (let ([dc (get-dc)]) (repaint dc)))
      (define/override (on-event event)
        (when (send event dragging?)
          (set-field! width this (- (send event get-x) self-padding))
          (refresh-now repaint)))))

  (define p1 (new horizontal-pane% [parent frame] [spacing 10]))
  (define canvas (new my-canvas% [parent p1] [min-width 1000]))
  (define p2 (new vertical-pane% [parent p1] [spacing 40] [vert-margin 30] [horiz-margin 10]))

  #;(define leaf-node-slider (new slider% [label "leaf-node weight ratio"]
                                [style '(horizontal plain vertical-label)]
                                [min-value -100] [max-value 200] [init-value 50] [parent p2]
                                [callback (lambda (s e) (send canvas set-leaf-node-weight-ratio (send s get-value)))]))
  #;(define padding-slider (new slider% [label "internal padding"]
                              [style '(horizontal plain vertical-label)]
                              [min-value 0] [max-value 30] [init-value 10] [parent p2]
                              [callback (lambda (s e) (send canvas set-padding (send s get-value)))]))

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
         [choices justify-content-choices]
         [parent p4]
         [style '(vertical vertical-label)]
         [callback
          (λ (self e)
            (send canvas set-justify-content
                  (case (list-ref justify-content-choices (send self get-selection))
                    [("space-evenly") jc-space-evenly]
                    [("space-between") jc-space-between]
                    [("space-around") jc-space-around]
                    [("start") jc-start]
                    [("end") jc-end]
                    [("left") jc-left]
                    [("center") jc-center]
                    [("right") jc-right])))]))
  (define align-items-choices
    (list "top" "center" "bottom" "baseline"))
  (define align-items-radio
    (new radio-box%
         [label "align-items"]
         [choices align-items-choices]
         [parent p4]
         [style '(vertical vertical-label)]
         [callback
          (λ (self e)
            (send canvas set-align-items
                  (case (list-ref align-items-choices (send self get-selection))
                    [("top") ai-top]
                    [("center") ai-center]
                    [("bottom") ai-bottom]
                    [("baseline") ai-baseline])))]))

  (define expr-submit
    (new button%
         [label "submit"]
         [parent p3]
         [callback
          (λ (b e)
            (send canvas set-expr (read (open-input-string (send expr-textbox get-value)))))]))

  (send frame show #t))
