#lang racket
(require racket/draw racket/help)
(require (for-syntax racket/syntax))
(provide
 align-items center
 justify-content space-evenly flex-end
 min-strut-width row-gap
 the-font the-font-size
 the-atom-text-pen the-atom-box-pen the-strut-pen the-atom-box-brush
 layout% text-box% hstrut% happend-layout% ellipsis-marker%
 vappend-inline-layout% vappend-block-layout% vappend-forward-backward-layout%
 diagram% block-diagram% stack%
 inline-diagram% sequence% wrapped-sequence% station% epsilon%)


(define (display-expr k) (begin (displayln k) k))

;; a type of align-items
(define (center total-width this-width)
  (/ (- total-width this-width) 2))

;; a type of justify-content
; contract: must not exceed total-space
; contract: total-space must be at least (* min-gap num-subs)
(define (space-evenly total-space num-subs [min-gap 0])
  (let* ([each-basis (/ total-space (+ num-subs 1))]
         [each-mid-space (max min-gap each-basis)]
         [each-end-space (/ (- total-space (* each-mid-space (- num-subs 1))) 2)])
    (append (list each-end-space)
            (build-list (- num-subs 1) (lambda _ each-mid-space))
            (list each-end-space))))

;; a type of justify-content
(define (flex-end total-space num-subs [min-gap 0])
  (append
   (list (- total-space (* min-gap (- num-subs 1))))
   (build-list (- num-subs 1) (λ _ min-gap))
   (list 0)))


;; affect layout
(define align-items (make-parameter center))
(define justify-content (make-parameter space-evenly))

(define the-font
  (make-parameter (make-font #:font-list the-font-list
                             #:size 12
                             #:face "Ubuntu Mono"
                             #:family 'modern
                             #:style 'normal
                             #:weight 'normal)))
(define the-font-size
  (make-derived-parameter
   the-font
   (lambda (fs) (make-font #:font-list the-font-list
                           #:size fs
                           #:face (send (the-font) get-face)
                           #:family (send (the-font) get-family)
                           #:style (send (the-font) get-style)
                           #:weight (send (the-font) get-weight)))
   (lambda (f) (send f get-size))))

(define text-measurement-dc (new bitmap-dc% [bitmap #f]))

(define (text-width text)
  (send text-measurement-dc set-font (the-font))
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    width))

(define (text-height text)
  (send text-measurement-dc set-font (the-font))
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    height))

; not parameters; global constants
(define min-strut-width 6)
(define row-gap 8)

;; affect rendering
(define the-atom-text-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-atom-box-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-strut-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-atom-box-brush
  (make-parameter (make-brush #:style 'transparent)))


;; A layout can have a tip at height:
;;   * 'top                 an upwards tip
;;   * (logical . [0,H])    integers are exactly at logical rows, and other
;;                          numbers are logically interpolated such that 0.5
;;                          is halfway between the sublayouts on rows 0 and 1
;;                          (always a horizontal tip)
;;   * 'bot                 a downwards tip
;;   * 'default             the default among the above
;;
;; These are on both sides (left and right) of the layout.
(define layout%
  (class object% (super-new)
    (init-field physical-width physical-height
                ; ((left . n) (right . n))
                num-rows
                ; ((left spec . y) (right spec . y))
                tips
                [direction 'ltr])
    (define/pubment tip-y
      (case-lambda
        [(side) (cddr (assoc side tips))]
        [(side spec) (inner #f tip-y side spec)]))
    (abstract render #;(render x y))))

(define (direction-toggle d)
  (case d [(ltr) 'rtl] [(rtl) 'ltr] [else (raise-argument-error 'd "direction?" d)]))

(define (vappend-common %)
  (class %
    (init tip-specs)
    (init-field [(subs common-subs)])
    (define side-struts?
      (for/list ([side '(left right)])
        (cons side (not (memq (cdr (assoc side tip-specs)) '(top bot none))))))

    (let ([sub-widths (map (lambda (s) (get-field physical-width s)) subs)])
      ; inexact equality
      (unless (> 0.0001 (- (apply max sub-widths) (apply min sub-widths)))
        (raise-arguments-error 'vappend-layout "subs must be equal widths"
                               "sub-widths" sub-widths))
      (super-new
       [physical-width (+ (first sub-widths)
                          (if (cdr (assoc 'left side-struts?)) min-strut-width 0)
                          (if (cdr (assoc 'right side-struts?)) min-strut-width 0))]
       [physical-height
        (+ (apply + (map (lambda (s) (get-field physical-height s)) subs))
           (* (- (length subs) 1) row-gap))]))

    (inherit tip-y)
    (inherit-field physical-width direction)

    (define/public (check-directions)
      (unless (andmap (lambda (s) (eq? (get-field direction s) direction)) subs)
        (raise-arguments-error
         'vappend-layout
         "subs must all have same direction as this layout"
         "subs" subs "direction" direction)))
    (check-directions)

    (define/overment (render x y)
      (let ([sub-x (+ x (if (cdr (assoc 'left side-struts?)) min-strut-width 0))])
        (let-values
            ([(sub-renders sub-ys)
              (for/fold
                  ([sub-y y]
                   [sub-renders '()]
                   [sub-ys '()]
                   #:result (values (apply append sub-renders)
                                    (reverse sub-ys)))
                  ([sub subs])
                (values (+ sub-y (get-field physical-height sub) row-gap)
                        (cons (send sub render sub-x sub-y) sub-renders)
                        (cons sub-y sub-ys)))])
          (append
           sub-renders
           `((set-pen ,(the-strut-pen)))
           (for/list ([side '(left right)]
                      [tip-x (list x (+ x physical-width (- min-strut-width)))]
                      #:when (cdr (assoc side side-struts?)))
             `(draw-line ,tip-x ,(+ y (tip-y side))
                         ,(+ tip-x min-strut-width) ,(+ y (tip-y side))))
           (inner '() render x y sub-x (get-field physical-width (first subs)) sub-ys)))))))

(define (linear-interpolate xstart ystart xend yend x)
  (+ ystart (* (/ (- x xstart) (- xend xstart)) (- yend ystart))))

(define vappend-block-layout%
  (class (vappend-common layout%)
    (init tip-specs)
    (init-field subs)

    (define ((get-rows side) sub)
      (if (and (is-a? sub vappend-block-layout%)
               (not (memq (cadr (assoc side (get-field tips sub)))
                          '(top bot))))
          1
          (cdr (assoc side (get-field num-rows sub)))))
    (define num-rows
      (for/list ([side '(left right)])
        (cons side (apply + (map (get-rows side) subs)))))

    (define/augride (tip-y side spec)
      (match spec
        ['top (send (first subs) tip-y side spec)]
        ['bot (let ([last-sub (last subs)])
                (+ (apply + (map (lambda (s) (get-field physical-height s)) subs))
                   (* (- (length subs) 1) row-gap)
                   (- (get-field physical-height last-sub))
                   (send last-sub tip-y side spec)))]
        [(cons 'logical (? number? row-num))
         (let* ([num-rows (- (cdr (assoc side num-rows)) 1)])
           (unless (<= 0 row-num num-rows)
             (raise-arguments-error
              'vappend-block-layout-tip-y
              "logical tip spec must be in [0, R-1]"
              "height" row-num "L" num-rows))
           (let loop ([n row-num] [cumul-y (- (/ row-gap 2))] [subs subs])
             (let* ([sub (first subs)]
                    [sub-rows (- ((get-rows side) sub) 1)]
                    [sub-top-y (+ cumul-y (/ row-gap 2))]
                    [next-cumul-y
                     (+ sub-top-y (get-field physical-height sub) (/ row-gap 2))])
               (cond
                 [(< n 0)
                  (linear-interpolate
                   -0.5 cumul-y
                   0 (+ sub-top-y (send sub tip-y side '(logical . 0)))
                   n)]
                 [(<= 0 n sub-rows)
                  (+ sub-top-y (send sub tip-y side `(logical . ,n)))]
                 [(< n (+ sub-rows 0.5))
                  (linear-interpolate
                   sub-rows (+ sub-top-y (send sub tip-y side `(logical . ,sub-rows)))
                   (+ sub-rows 0.5) next-cumul-y
                   n)]
                 [else (loop (- n sub-rows 1) next-cumul-y (rest subs))]))))]
        ['default
         (tip-y side (cons 'logical
                           (quotient (- (cdr (assoc side num-rows)) 1) 2)))]
        [else #f]))

    (super-new
     [tip-specs tip-specs] [common-subs subs] [num-rows num-rows]
     [tips (map (lambda (s)
                  (let ([side (car s)] [spec (cdr s)])
                    (cons side (cons spec (tip-y side spec)))))
                tip-specs)])

    (inherit-field direction tips)

    (define/augride (render x y sub-x sub-width sub-ys)
      (cons
       `(set-pen ,(the-strut-pen))
       (let ([top-sub (first subs)]
             [top-y (first sub-ys)]
             [bot-sub (last subs)]
             [bot-y (last sub-ys)]
             [arc-size (* 2 min-strut-width)])
         (apply
          append
          (for/list ([side '(left right)]
                     [bracket-x (list sub-x (+ sub-x (get-field physical-width (first subs))))]
                     [arc-x (list 0 (- arc-size))]
                     [arc-theta (list (/ pi 2) 0)])
            `((draw-line ,bracket-x ,(+ top-y (send top-sub tip-y side) (/ arc-size 2))
                         ,bracket-x ,(+ bot-y (send bot-sub tip-y side) (- (/ arc-size 2))))
              #;(draw-arc
               ,(+ bracket-x arc-x) ,(+ top-y (send top-sub tip-y side))
               ,arc-size ,arc-size
               ,arc-theta ,(+ arc-theta (/ pi 2)))
              #;(draw-arc
               ,(+ bracket-x arc-x) ,(+ bot-y (send bot-sub tip-y side) (- arc-size))
               ,arc-size ,arc-size
               ,(- (- arc-theta) (/ pi 2)) ,(- arc-theta))))))))))

(define vappend-forward-backward-layout%
  (class vappend-block-layout%
    (init ([internal-subs subs]))
    (unless (= (length internal-subs) 2)
      (raise-arguments-error
       'vappend-forward-backward-layout
       "vappend-forward-backward must have exactly 2 subs"
       "subs" internal-subs))

    (define/override (tip-y side spec)
      (match spec
        ['default (super tip-y side '(logical . 0))]
        [else (super tip-y side spec)]))

    (super-new [subs internal-subs])

    (inherit-field direction subs)
    (define/override (check-directions)
      (unless (eq? direction (get-field direction (first subs)))
        (raise-arguments-error
         'vappend-forward-backward-layout
         "first sub must have same direction as this layout"
         "first sub" (first subs) "direction" direction))
      (unless (eq? (direction-toggle direction) (get-field direction (second subs)))
        (raise-arguments-error
         'vappend-forward-backward-layout
         "second sub must have opposite direction as this layout"
         "second sub" (second subs) "direction" direction)))))

(define inline-layout%
  (class layout%
    (init tips [num-rows '((left . 1) (right . 1))])
    (unless (and (eq? 'default (cadr (assoc 'left tips)))
                 (eq? 'default (cadr (assoc 'right tips))))
      (raise-arguments-error 'inline-layout "tip specs must be 'default"
                             "tips" tips))
    (super-new [num-rows num-rows] [tips tips])
    (define/augment (tip-y side spec)
      (or (inner #f tip-y side spec)
          (case spec
            [(default (logical . 0) top bot) (tip-y side)]
            [else #f])))))

(define vappend-inline-layout%
  (class (vappend-common inline-layout%)
    (init-field subs style [marker #f])
    ; needed for modifying subs if style is marker below, before super-new.
    ; renamed internally so that after super-new we can inherit "direction".
    (init [(init-direction direction) 'ltr])

    (case style
      [(marker)
       (unless (is-a? marker inline-layout%)
         (raise-arguments-error
          'vappend-inline-layout
          "when style is 'marker, marker must be an inline-layout%"
          "marker" marker))
       (unless (= (length subs) 1)
         (let*-values ([(first-subs rest-subs) (split-at subs 1)]
                       [(mid-subs last-subs) (split-at-right rest-subs 1)])
           (let* ([marker-width (get-field physical-width marker)]
                  [struct-sub-markers
                   (for/list ([sub (list (first first-subs) (first last-subs))]
                              [maybe-reverse (if (eq? init-direction 'rtl)
                                                 (list reverse identity)
                                                 (list identity reverse))])
                     (new happend-layout% [fuse? #t] [direction init-direction]
                          [subs (maybe-reverse
                                 (list (new hstrut% [physical-width marker-width]
                                            [direction init-direction] [always-arrow #t])
                                       sub marker))]))])
             (set! subs (append
                         (list (first struct-sub-markers))
                         (map (λ (s) (new happend-layout% [direction init-direction]
                                          [subs (list marker s marker)]))
                              mid-subs)
                         (list (second struct-sub-markers)))))))]

      [(boustrophedon)
       (unless (odd? (length subs))
         (raise-arguments-error
          'vappend-inline-layout
          "when style is 'boustrophedon, must have odd number of subs"
          "subs" subs))]

      [(bare) 'pass]

      [else
       (raise-arguments-error
        'vappend-inline-layout "style must be 'bare, 'marker, or 'boustrophedon"
        "style" style)])

    (super-new
     [common-subs subs] [direction init-direction]
     ;; so that vappend-common doesn't draw tips
     [tip-specs '((left . none) (right . none))]
     [tips
      (let ([start-tip (λ (side) (send (first subs) tip-y side))]
            [end-tip (λ (side)
                       (let-values ([(nonlast-subs last-sub) (split-at-right subs 1)])
                         (+ (send (first last-sub) tip-y side)
                            (apply + (map (lambda (s) (get-field physical-height s))
                                          nonlast-subs))
                            (* (length nonlast-subs) row-gap))))])
        (if (eq? init-direction 'ltr)
            `((left default . ,(start-tip 'left)) (right default . ,(end-tip 'right)))
            `((left default . ,(end-tip 'left)) (right default . ,(start-tip 'right)))))])

    (inherit-field direction)

    (define/override (check-directions)
      (if (eq? style 'boustrophedon)
          (let* ([directions (map (lambda (s) (get-field direction s)) subs)]
                 [first-direction (first directions)])
            (unless (andmap (lambda (d i) (xor (odd? i) (eq? d first-direction)))
                            directions (range (length directions)))
              (raise-arguments-error
               'vappend-inline-layout
               "when style is 'boustrophedon, subs must have alternating directions"
               "subs" subs
               "directions" directions))
            (unless (eq? first-direction direction)
              (raise-arguments-error
               'vappend-inline-layout
               "when style is 'boustrophedon, first sub must have same direction as this layout"
               "first direction" first-direction
               "this direction" direction)))
          (super check-directions)))

    (define/augride (render x y sub-x sub-width sub-ys)
      (if (eq? style 'boustrophedon)
          (for/fold ([brackets '()]
                     [end-right? (eq? (get-field direction (first subs)) 'ltr)]
                     #:result (cons `(set-pen ,(the-strut-pen)) brackets))
                    ([sub subs]
                     [sub-y sub-ys]
                     [next-sub (rest subs)]
                     [next-y (rest sub-ys)])
            (values
             (cons
              (if end-right?
                  `(draw-line ,(+ sub-x sub-width) ,(+ sub-y (send sub tip-y 'right))
                              ,(+ sub-x sub-width) ,(+ next-y (send next-sub tip-y 'right)))
                  `(draw-line ,sub-x ,(+ sub-y (send sub tip-y 'left))
                              ,sub-x ,(+ next-y (send next-sub tip-y 'left))))
              brackets)
             (not end-right?)))
          '()))))

(define hspace%
  (class inline-layout%
    (init [(init-width physical-width) min-strut-width])
    (super-new [physical-height 0]
               [tips '((left default . 0) (right default . 0))]
               [physical-width init-width] #;[direction pass-through])
    (define/override (render x y) '())))

(define hstrut%
  (class hspace%
    (init-field [always-arrow #f])
    (super-new #;[physical-width pass-through] #;[direction pass-through])
    (inherit-field physical-width direction)
    (define/override (render x y)
      (append
       `((set-pen ,(the-strut-pen))
         (draw-line ,x ,y ,(+ x physical-width) ,y))
       (if (or always-arrow (>= physical-width (* min-strut-width 7)))
           (let* ([base-diff (* (the-font-size) 0.125)]
                  [x-diff ((if (eq? direction 'ltr) + -) (* base-diff 3))]
                  [y-diff (* base-diff 2)])
             `((draw-lines
                ((,(- x-diff) . ,(- y-diff)) (,x-diff . 0) (,(- x-diff) . ,y-diff))
                                           ; optical correction
                ,(+ x (/ physical-width 2) (* x-diff 0.3)) ,y)))
           '())))
    (define/public (fuse other)
      (unless (is-a? other hstrut%)
        (raise-arguments-error 'hstrut "can only fuse with another hstrut" "other" other))
      (unless (eq? direction (get-field direction other))
        (raise-arguments-error 'hstrut "can only fuse with hstrut in same direction"))
      (new hstrut% [direction direction]
           [physical-width (+ physical-width (get-field physical-width other))]
           [always-arrow (or always-arrow (get-field always-arrow other))]))))

(define text-box%
  (class inline-layout%
    (init-field terminal? label [padding-x (/ (the-font-size) 2)]
                [padding-y (/ (the-font-size) 3)])
    (define label-width (text-width label))
    (define label-height (text-height label))
    (let ([physical-height (+ label-height (* 2 padding-y))])
      (super-new
       [physical-width (+ label-width (* 2 padding-x))]
       [physical-height physical-height]
       [tips `((left default . ,(/ physical-height 2))
               (right default . ,(/ physical-height 2)))]))
    (inherit-field physical-height)
    (define/override (render x y)
      (let* ([box-width (+ label-width (* 2 padding-x))]
             [box-height physical-height]
             [box-x x]
             [box-y y]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y padding-y)]
             [struts-y (+ y (/ physical-height 2))]
             [lstrut-lx x]
             [lstrut-rx (+ lstrut-lx min-strut-width)]
             [rstrut-lx (+ x min-strut-width box-width)]
             [rstrut-rx (+ rstrut-lx min-strut-width)])
        `((set-pen ,(the-atom-text-pen))
          (set-font ,(the-font))
          (draw-text ,label ,text-x ,text-y #t)
          (set-pen ,(the-atom-box-pen))
          (set-brush ,(the-atom-box-brush))
          (,(if terminal? 'draw-rounded-rectangle 'draw-rectangle)
           ,box-x ,box-y ,box-width ,box-height))))))

(define text-marker%
  (class text-box%
    (super-new [terminal? #f] [padding-x min-strut-width])
    (define y-alignment-magic (* (the-font-size) 0.23))
    (inherit-field label padding-x)
    (define/override (render x y)
      (let* ([box-x x]
             [box-y y]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y y-alignment-magic)])
        `((set-pen ,(the-atom-text-pen))
          (set-font ,(the-font))
          (draw-text ,label ,text-x ,text-y #t))))))

(define ellipsis-marker%
  (class text-marker%
    (super-new [label "…"])))

(define random-marker%
  (class text-marker%
    (super-new [label (list-ref '("†" "‡" "◊" "¤" "*") (random 5))])))

(define happend-layout%
  (class inline-layout%
    (init [(init-subs subs)] [fuse? #f])
    (field
     [subs
      (if fuse?
          (let ([spliced-subs
                 (apply append
                        (map (λ (s) (if (is-a? s happend-layout%) (get-field subs s) (list s)))
                             init-subs))])
            (for/fold ([cur-hstrut #f]
                       [subs '()]
                       #:result (reverse (if cur-hstrut (cons cur-hstrut subs) subs)))
                      ([cur-sub spliced-subs])
              (if cur-hstrut
                  (if (is-a? cur-sub hstrut%)
                      (values (send cur-hstrut fuse cur-sub) subs)
                      (values #f (cons cur-sub (cons cur-hstrut subs))))
                  (if (is-a? cur-sub hstrut%)
                      (values cur-sub subs)
                      (values #f (cons cur-sub subs))))))
          init-subs)])

    (define expose-first-left-tips?
      (memq (cadr (assoc 'left (get-field tips (first subs)))) '(top bot)))
    (define expose-last-right-tips?
      (memq (cadr (assoc 'right (get-field tips (last subs)))) '(top bot)))

    (unless (>= (length subs) 1)
      (raise-arguments-error
       'happend-layout
       "happend-layout must have at least one sub layout"))

    (define/augride (tip-y side spec)
      (match spec
        [(cons 'logical _)
         (cond
           [(and (eq? side 'left) expose-first-left-tips?)
            (send (first subs) tip-y side spec)]
           [(and (eq? side 'right) expose-last-right-tips?)
            (send (last subs) tip-y side spec)]
           [else #f])]
        [else #f]))

    (define-values (right-tip height sub-xs width)
      (for/fold ([prev-right-tip-y 0]
                 [prev-height 0]
                 [prev-widths '(0)]
                 #:result (values prev-right-tip-y prev-height
                                  (reverse (cdr prev-widths))
                                  (car prev-widths)))
                ([sub subs])
        (let ([sub-height (get-field physical-height sub)]
              [sub-width (get-field physical-width sub)]
              [left-tip-y (send sub tip-y 'left)]
              [right-tip-y (send sub tip-y 'right)])
          ;
          ;                        given           wanted
          ;                     measurements    measurements
          ;
          ; o------o                =  =           =  =
          ; |      |    o----o      |  |           |  |
          ; |      r-  -l    |      |  =           |  |
          ; |      |    |    r-     |              |  =
          ; o------o    |    |      =              |
          ;             o----o                     =
          ;
          (values (+ right-tip-y
                     (max 0 (- prev-right-tip-y left-tip-y)))
                  (+ prev-height
                     (max 0 (- left-tip-y prev-right-tip-y))
                     (max 0 (- (- sub-height left-tip-y)
                               (- prev-height prev-right-tip-y))))
                  (cons (+ sub-width (car prev-widths)) prev-widths)))))
    (define-values (left-tip sub-ys)
      (for/foldr ([prev-left-tip-y right-tip]
                  [prev-ys '()])
                 ([sub subs])
        (let ([left-tip-y (send sub tip-y 'left)]
              [right-tip-y (send sub tip-y 'right)])
          (let ([sub-y (- prev-left-tip-y right-tip-y)])
            (values (+ sub-y left-tip-y) (cons sub-y prev-ys))))))
    (super-new
     [physical-width width]
     [physical-height height]
     [tips `((left default . ,left-tip) (right default . ,right-tip))]
     [num-rows `((left . ,(if expose-first-left-tips?
                              (cdr (assoc 'left (get-field num-rows (first subs))))
                              1))
                 (right . ,(if expose-last-right-tips?
                               (cdr (assoc 'right (get-field num-rows (last subs))))
                               1)))])

    (inherit-field direction)
    (unless (andmap (lambda (s) (eq? (get-field direction s) direction)) subs)
      (raise-arguments-error
       'happend-layout
       "subs must all have same direction as this layout"
       "subs" subs "direction" direction))

    (define/override (render x y)
      (append-map (lambda (sub sx sy) (send sub render (+ x sx) (+ y sy)))
                  subs sub-xs sub-ys))))



(define diagram%
  (class object% (super-new)
    (init-field min-content max-content flex)

    ;; Attempt to lay out the diagram with the requested tips and width. Will
    ;; always succeed, but the resulting tips and widths may not be what were
    ;; requested, even if it would have been possible.
    ;
    ; to SVG? HTML? TiKZ? how to have different backends?
    (#;(lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
     abstract lay-out)))

(define block-diagram%
  (class diagram% (super-new)))

(define (justify-layouts-without-zero-hstruts justify-space layouts direction [min-gap 0])
  (let* ([justify-lengths ((justify-content) justify-space (length layouts) min-gap)]
         [layouts-and-struts
          (let loop ([los layouts] [jls justify-lengths])
            (if (empty? jls) '()
                (let ([rests (if (empty? los) '()
                                 (cons (car los) (loop (cdr los) (cdr jls))))])
                  (if (zero? (car jls)) rests
                      (cons (new hstrut% [physical-width (car jls)] [direction direction])
                            rests)))))])
    (new happend-layout% [subs layouts-and-struts] [fuse? #t] [direction direction])))

(define (directional-reverse direction l)
  ((if (eq? direction 'rtl) reverse identity) l))

(define stack%
  (class block-diagram%
    (init-field diag-top diag-bot polarity)
    (init [(init-flex flex) #t])
    (unless (memq polarity '(+ -))
      (raise-arguments-error 'stack "polarity must be '+ or '-" "polarity" polarity))

    (super-new
     [min-content (max (get-field min-content diag-top) (get-field min-content diag-bot))]
     [max-content (max (get-field max-content diag-top) (get-field max-content diag-bot))]
     [flex init-flex])
    (inherit-field min-content max-content flex)

    (define/override (lay-out width [start-tip 'default] [end-tip 'default] [direction 'ltr])
      (let* ([start-tip-width (if (memq start-tip '(top bot)) 0 min-strut-width)]
             [end-tip-width (if (memq end-tip '(top bot)) 0 min-strut-width)]
             [available-width (- width start-tip-width end-tip-width)]
             [clamped-available-width
              ((if flex (λ (a b) b) min) max-content (max min-content available-width))]
             [layout-top (send diag-top lay-out clamped-available-width 'bot 'bot direction)]
             [layout-bot (send diag-bot lay-out clamped-available-width 'top 'top
                               (if (eq? polarity '+) direction (direction-toggle direction)))]
             [justified-layouts
              (map
               (λ (layout)
                (justify-layouts-without-zero-hstruts
                 (- clamped-available-width (get-field physical-width layout))
                 (list layout)
                 (get-field direction layout)))
               (list layout-top layout-bot))])
        (new (if (eq? polarity '+) vappend-block-layout% vappend-forward-backward-layout%)
             [subs justified-layouts] [direction direction]
             [tip-specs
              (map cons '(left right) (directional-reverse direction `(,start-tip ,end-tip)))])))))

(define station%
  (class block-diagram%
    (init-field terminal? label)
    (define init-label-width
      (+ (* 2 min-strut-width)
         (get-field physical-width (new text-box% [terminal? terminal?] [label label]))))
    (super-new [min-content init-label-width] [max-content init-label-width] [flex #f])

    (define/override (lay-out width [start-tip 'default] [end-tip 'default] [direction 'ltr])
      ;; requested tips and width don't matter
      (let ([tip-strut (new hstrut% [physical-width min-strut-width] [direction direction])]
            [text-box (new text-box% [terminal? terminal?] [label label] [direction direction])])
        (new happend-layout% [subs (list tip-strut text-box tip-strut)] [direction direction])))))

(define epsilon%
  (class block-diagram%
    (super-new [min-content 0] [max-content 0] [flex #t])
    (define/override (lay-out width [start-tip 'default] [end-tip 'default] [direction 'ltr])
      (new hstrut% [physical-width width] [direction direction]))))

(define inline-diagram%
  (class diagram% (super-new)))

(define (break-at lst idxs)
  (if (empty? idxs)
      (list lst)
      (let helper ([lst lst]
                   [didxs (map (λ (i prev-i) (- i prev-i)) idxs (cons -1 (drop-right idxs 1)))])
        (match didxs
          ['() (list lst)]
          [(cons idx rests) (cons (take lst idx) (helper (drop lst idx) rests))]))))

(define sequence%
  (class inline-diagram%
    (init-field subs [min-gap 0] [flex-absorb 0.2])
    (unless (> (length subs) 0)
      (raise-arguments-error 'sequence "must sequence at least one diagram"))

    (define wraps-measures
      (map
       (λ (wrap-spec)
         (let ([wrapped
                (new wrapped-sequence% [subs subs] [wrap-spec wrap-spec]
                     [min-gap min-gap] [flex-absorb flex-absorb])])
           (list (get-field min-content wrapped) (get-field max-content wrapped) wrapped)))
       (combinations (range (- (length subs) 1)))))

    (super-new [min-content (first (last wraps-measures))]
               [max-content (second (first wraps-measures))]
               [flex #t])
    (inherit-field min-content max-content)

    (define/override (lay-out width [start-tip 'default] [end-tip 'default] [direction 'ltr])
      ;; requested tips don't matter
      (send (cond
              [(= width min-content) (third (last wraps-measures))]
              [(= width max-content) (third (first wraps-measures))]
              [else
               (let ([fitting (filter (λ (wm) (<= (second wm) width)) wraps-measures)])
                 (third
                  (if (empty? fitting) (argmin first wraps-measures) (argmax second fitting))))])
            lay-out width start-tip end-tip direction))))

(define (+map f . ls) (apply + (apply map f ls)))

(define wrapped-sequence%
  (class inline-diagram%
    (init-field subs wrap-spec [min-gap 0] [flex-absorb 0.2])
    (unless (andmap (λ (break) (<= 0 break (- (length subs) 2))) wrap-spec)
      (raise-arguments-error
       'wrapped-sequence "wrap-spec breaks must be in [0, (- (length subs) 2)]"))
    (define wrapped-subs (break-at subs wrap-spec))

    (define maybe-marker-width
      (if (empty? wrap-spec) 0 (* 2 (get-field physical-width (new ellipsis-marker%)))))
    (define (sum-content field-name)
      (+ (apply max (map (λ (row) (+ (+map (λ (s) (dynamic-get-field field-name s)) row)
                                     (* (- (length row) 1) min-gap)
                                     (* 2 (count (is-a?/c stack%) row) min-strut-width)))
                         wrapped-subs))
         maybe-marker-width))
    (super-new [min-content (sum-content 'min-content)]
               [max-content (sum-content 'max-content)]
               [flex #t])
    (inherit-field min-content)

    (define/override (lay-out width [start-tip 'default] [end-tip 'default] [direction 'ltr])
      ;; requested tips don't matter
      (let ([available-width (- (max width min-content) maybe-marker-width)])
        (new
         vappend-inline-layout% [direction direction]
         [subs
          (map
           (λ (row)
             (let* (; available-width will be rebound at every step
                    [available-width (- available-width (* min-gap (- (length row) 1)))]
                    ; w- bindings will add up to the width for each sub
                    [w-min-contents (map (λ (s) (get-field min-content s)) row)]
                    [w-stack-struts
                     (map (λ (s) (if (is-a? s stack%) (* 2 min-strut-width) 0)) row)]
                    [available-width
                     (- available-width (apply + w-min-contents) (apply + w-stack-struts))]
                    [remaining-contents (map (λ (s mc) (- (get-field max-content s) mc))
                                             row w-min-contents)]
                    [total-remaining-contents (apply + remaining-contents)]
                    [w-grow-content
                     (if (= total-remaining-contents 0) (make-list (length row) 0)
                         (map (λ (remc)
                                (min remc (* available-width (/ remc total-remaining-contents))))
                              remaining-contents))]
                    [available-width (- available-width (apply + w-grow-content))]
                    ; x- bindings will add up to the absorbed space
                    [x-initial (* flex-absorb available-width)]
                    [available-width (- available-width x-initial)]
                    [flex-max-contents
                     (map (λ (s) (if (get-field flex s) (get-field max-content s) 0)) row)]
                    [total-flex-max-contents (apply + flex-max-contents)]
                    [w-grow-flex
                     (if (= total-flex-max-contents 0) (make-list (length row) 0)
                         (map (λ (fmc) (* available-width (/ fmc total-flex-max-contents)))
                              flex-max-contents))]
                    [x-post-flex (- available-width (apply + w-grow-flex))]
                    [w-total (map + w-min-contents w-stack-struts w-grow-content w-grow-flex)]
                    ; TODO tips
                    [sub-layouts
                     (map (λ (s w) (send s lay-out w 'default 'default direction)) row w-total)])
               (justify-layouts-without-zero-hstruts
                (+ x-initial x-post-flex)
                (directional-reverse direction sub-layouts)
                direction min-gap)))
           wrapped-subs)]
         [style 'marker] [marker (new ellipsis-marker% [direction direction])])))))
