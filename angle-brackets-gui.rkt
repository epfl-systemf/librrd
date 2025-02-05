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
             [width 600] [self-padding 10])
      (define/public (set-expr value)
        (set-field! expr this value)
        (set-field! diag this (diagram value))
        (refresh-now repaint))
      #;(define/public (set-leaf-node-weight-ratio value)
        (set-field! leaf-weight this (expt 10 (- 1 (/ value 50))))
        (refresh-now repaint))
      #;(define/public (set-padding value)
        (set-field! padding this value)
        (refresh-now repaint))
      (define (repaint dc)
        (for-each
         (λ (render) (for-each (λ (cmd) (apply dynamic-send dc cmd)) render))
         (list
          (send (send diag lay-out width) render self-padding self-padding)
          (let ([l (send diag lay-out-global width)])
            (parameterize ([the-atom-box-pen (make-pen #:color "red" #:width 1 #:style 'solid)]
                           [the-strut-pen (make-pen #:color "red" #:width 1 #:style 'solid)])
              (send l render self-padding (+ self-padding 500)))))))
      (define/override (on-paint) (let ([dc (get-dc)]) (repaint dc)))
      (define/override (on-event event)
        (when (send event dragging?)
          (set-field! width this (- (send event get-x) self-padding))
          #;(set-field! height this (- (send event get-y) self-padding))
          (refresh-now repaint)))))

  (define p1 (new horizontal-pane% [parent frame] [spacing 10]))
  (define canvas (new my-canvas% [parent p1] [min-width 700]))
  (define p2 (new vertical-pane% [parent p1] [spacing 40] [vert-margin 20] [horiz-margin 10]))

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
  (define expr-submit
    (new button%
         [label "submit"]
         [parent p3]
         [callback
          (λ (b e)
            (send canvas set-expr (read (open-input-string (send expr-textbox get-value)))))]))

  (send frame show #t))
