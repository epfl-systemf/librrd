#lang racket
(require racket/draw racket/help)
(require (for-syntax racket/syntax))

(define-syntax (with-destruct-object-as stx)
  (syntax-case stx ()
    [(_ obj (fields ...) body ...)
     (and (identifier? #'obj) (andmap identifier? (syntax->list #'(fields ...))))
     (let* ([prefixer (lambda (id) (format-id id "~a-~a" #'obj id))]
            [prefixed (map prefixer (syntax->list #'(fields ...)))])
       (with-syntax ([(names ...) prefixed])
         #'(let ([names (get-field fields obj)] ...)
             body ...)))]))

(define-syntax-rule (with-origin-delta dc dx dy body ... last-body)
  (let-values ([(orig-x orig-y) (send dc get-origin)])
    (send dc set-origin (+ orig-x dx) (+ orig-y dy))
    body ...
    (let ([last-body-val last-body])
      (send dc set-origin orig-x orig-y)
      last-body-val)))

(define (with-translate dx dy instructions)
  `((translate ,dx ,dy) ,@instructions (translate ,(- dx) ,(- dy))))

;; TODO: should be reversed in the expr before (diagram), right?
#;(define (diagram expr)
  (match expr
    ['(epsilon) #f]
    [(list 'term label)
     (new atomic-block-diagram% [terminal? #t] [label label] [direction 'ltr])]
    [(list 'nonterm label)
     (new atomic-block-diagram% [terminal? #f] [label label] [direction 'ltr])]
    [(list* 'seq exprs)
     (if (= (length exprs) 0)
         (raise-arguments-error 'diagram "seq must have at least one expression")
         (new happend-inline-diagram%
              [diags (map diagram exprs)]))]
    [(list '<> polarity expr1 expr2)
     (let* ([diag2 (diagram expr2)]
            [maybe-rev-diag2 (if (eq? polarity '-) (reverse-diagram diag2) diag2)])
       (new vappend-block-diagram%
            [diag-top (diagram expr1)]
            [diag-bot maybe-rev-diag2]))]))


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


(define (linear-interpolate xstart ystart xend yend x)
  (+ ystart (* (/ (- x xstart) (- xend xstart)) (- yend ystart))))

(define (in-range? low x high) (and (<= low x) (< x high)))


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
      (unless (apply = sub-widths)
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
           (inner
            '()
            render x y sub-x (get-field physical-width (first subs)) sub-ys)))))))

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

    (inherit-field direction)

    (define/augride (render x y sub-x sub-width sub-ys)
      (cons
       `(set-pen ,(the-strut-pen))
       (for/list ([top-sub subs]
                  [top-y sub-ys]
                  [bot-sub (rest subs)]
                  [bot-y (rest sub-ys)]
                  #:when #t
                  [side '(left right)]
                  [bracket-x (list sub-x (+ sub-x (get-field physical-width (first subs))))])
         `(draw-line ,bracket-x ,(+ top-y (send top-sub tip-y side))
                     ,bracket-x ,(+ bot-y (send bot-sub tip-y side))))))))

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
           (let* ([last-sub (first last-subs)]
                  [first-sub (first first-subs)]
                  [start-end-struct
                   (new hstrut% [physical-width (get-field physical-width marker)]
                        [direction init-direction])]
                  [directional-reverse (if (eq? init-direction 'rtl) reverse identity)])
             (set!
              subs
              (append
               (cons
                (new happend-layout%
                     [direction init-direction]
                     [subs (directional-reverse
                            (list start-end-struct first-sub marker))])
                (map (lambda (s) (new happend-layout%
                                      [direction init-direction]
                                      [subs (list marker s marker)])) mid-subs))
               (list
                (new happend-layout%
                     [direction init-direction]
                     [subs (directional-reverse
                            (list marker last-sub start-end-struct))])))))))]

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
      `((left default . ,(send (first subs) tip-y 'left))
        (right default . ,(let-values ([(nonlast-subs last-sub) (split-at-right subs 1)])
                            (+ (send (first last-sub) tip-y 'right)
                               (apply + (map (lambda (s) (get-field physical-height s))
                                             nonlast-subs))
                               (* (length nonlast-subs) row-gap)))))])

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

(define hstrut%
  (class inline-layout%
    (super-new [physical-height 0]
               [tips '((left default . 0) (right default . 0))]
               #;[physical-width pass-through])
    (inherit-field physical-width)
    (define/override (render x y)
      `((set-pen ,(the-strut-pen))
        (draw-line ,x ,y ,(+ x physical-width) ,y)))))

(define text-box%
  (class inline-layout%
    (init-field terminal? label [padding-x (/ (the-font-size) 2)]
                [padding-y (/ (the-font-size) 3)])
    (define label-width (text-width label))
    (define label-height (text-height label))
    (let ([physical-height (+ label-height (* 2 padding-y))])
      (super-new
       [physical-width (+ label-width (* 2 padding-x) (* 2 min-strut-width))]
       [physical-height physical-height]
       [tips `((left default . ,(/ physical-height 2))
               (right default . ,(/ physical-height 2)))]))
    (inherit-field physical-height)
    (define/override (render x y)
      (let* ([box-width (+ label-width (* 2 padding-x))]
             [box-height physical-height]
             [box-x (+ x min-strut-width)]
             [box-y y]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y padding-y)]
             [struts-y (+ y (/ physical-height 2))]
             [lstrut-lx x]
             [lstrut-rx (+ lstrut-lx min-strut-width)]
             [rstrut-lx (+ x min-strut-width box-width)]
             [rstrut-rx (+ rstrut-lx min-strut-width)])
        (append
         `((set-pen ,(the-atom-text-pen))
           (set-font ,(the-font))
           (draw-text ,label ,text-x ,text-y #t)
           (set-pen ,(the-atom-box-pen))
           (set-brush ,(the-atom-box-brush)))
         (list (list (if terminal? 'draw-rounded-rectangle 'draw-rectangle)
                     box-x box-y box-width box-height))
         `((draw-line ,lstrut-lx ,struts-y ,lstrut-rx ,struts-y)
           (draw-line ,rstrut-lx ,struts-y ,rstrut-rx ,struts-y)))))))

(define ellipsis-marker%
  (class text-box%
    (super-new [label "…"] [terminal? #f] [padding-x 0])
    (define y-alignment-magic (* (the-font-size) 0.12))
    (inherit-field label padding-x)
    (define/override (render x y)
      (let* ([box-x (+ x min-strut-width)]
             [box-y y]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y y-alignment-magic)])
        `((set-pen ,(the-atom-text-pen))
          (set-font ,(the-font))
          (draw-text ,label ,text-x ,text-y #t))))))

(define happend-layout%
  (class inline-layout%
    (init-field subs)
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


(define mylo1 (new text-box% [terminal? #t] [label "hh"]))
(define mylo2 (new happend-layout% [subs (list mylo1 mylo1 (new hstrut% [physical-width 10]) mylo1)]))
(define mylo3-cont
  (new vappend-inline-layout%
       [subs
        (list (new vappend-block-layout%
                   [subs (list (new happend-layout%
                                    [subs (list (new text-box% [terminal? #t] [label "abc"])
                                                (new text-box% [terminal? #f] [label "xy"])
                                                (new hstrut% [physical-width 10]))])
                               (new happend-layout%
                                    [subs (list (new hstrut% [physical-width 10])
                                                (new text-box% [terminal? #t] [label "xy"])
                                                (new text-box% [terminal? #f] [label "abc"]))]))]
                   [tip-specs '((left . default) (right logical . 0.5))])
              (new happend-layout%
                   [subs (list (new text-box% [terminal? #t] [label "abc"])
                               (new hstrut% [physical-width 22])
                               (new text-box% [terminal? #f] [label "xy"]))])
              (new happend-layout%
                   [subs (list (new hstrut% [physical-width 22])
                               (new text-box% [terminal? #t] [label "abc"])
                               (new text-box% [terminal? #f] [label "xy"]))])
              (new happend-layout%
                   [subs (list (new hstrut% [physical-width 11])
                               (new text-box% [terminal? #t] [label "abc"])
                               (new hstrut% [physical-width 11])
                               (new text-box% [terminal? #f] [label "xy"]))]))]
       [style 'marker]
       [marker (new ellipsis-marker%)]))

(define mylo3
  (new vappend-inline-layout%
       [subs
        (list (new vappend-block-layout%
                   [subs (list (new happend-layout%
                                    [subs (list (new text-box% [terminal? #t] [label "abc"])
                                                (new text-box% [terminal? #f] [label "xy"])
                                                (new hstrut% [physical-width 10]))])
                               (new happend-layout%
                                    [subs (list (new hstrut% [physical-width 10])
                                                (new text-box% [terminal? #t] [label "xy"])
                                                (new text-box% [terminal? #f] [label "abc"]))]))]
                   [tip-specs '((left . default) (right logical . 0.5))])
              (new happend-layout%
                   [subs (list (new text-box% [terminal? #t] [label "abc"] [direction 'rtl])
                               (new hstrut% [physical-width 22] [direction 'rtl])
                               (new text-box% [terminal? #f] [label "xy"] [direction 'rtl]))]
                   [direction 'rtl])
              (new happend-layout%
                   [subs (list (new hstrut% [physical-width 11])
                               (new text-box% [terminal? #t] [label "abc"])
                               (new hstrut% [physical-width 11])
                               (new text-box% [terminal? #f] [label "xy"]))])
              (new happend-layout%
                   [subs (list (new text-box% [terminal? #t] [label "abc"] [direction 'rtl])
                               (new hstrut% [physical-width 22] [direction 'rtl])
                               (new text-box% [terminal? #f] [label "xy"] [direction 'rtl]))]
                   [direction 'rtl])
              (new happend-layout%
                   [subs (list (new hstrut% [physical-width 11])
                               (new text-box% [terminal? #t] [label "abc"])
                               (new hstrut% [physical-width 11])
                               (new text-box% [terminal? #f] [label "xy"]))]))]
       [style 'boustrophedon]))


(define diagram%
  (class object% (super-new)
    ; used for horizontal space distribution,
    ; relative heuristic for natural width
    (init-field weight)

    ;; Attempt to lay out the diagram with the requested tips and width. Will
    ;; always succeed, but the resulting tips and widths may not be what were
    ;; requested, even if it would have been possible.
    ;
    ; to SVG? HTML? TiKZ? how to have different backends?
    (#;(lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
     abstract lay-out)))

(define block-diagram%
  (class diagram% (super-new)))

(define (set-random-color! dc)
  (send dc set-pen (make-color (random 256) (random 256) (random 256)) 1 'solid))

(define stack%
  (class block-diagram%
    (init-field diag-top diag-bot polarity)
    (unless (memq polarity '(+ -))
      (raise-arguments-error 'stack "polarity must be '+ or '-"
                             "polarity" polarity))
    
    (super-new [weight (+ (max (get-field weight diag-top) (get-field weight diag-bot))
                          (* 4 min-strut-width))])

    (define/override (lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
      (let* ([left-tip-width (if (memq left-tip '(top bot)) 0 min-strut-width)]
             [right-tip-width (if (memq right-tip '(top bot)) 0 min-strut-width)]
             [sub-width (- width left-tip-width right-tip-width)]
             [prelim-layout-top (send diag-top lay-out sub-width 'bot 'bot direction)]
             [direction-bot
              (if (eq? polarity '+) direction (direction-toggle direction))]
             [prelim-layout-bot (send diag-bot lay-out sub-width 'top 'top direction-bot)]
             [effective-width-top (get-field physical-width prelim-layout-top)]
             [effective-width-bot (get-field physical-width prelim-layout-bot)]
             [effective-sub-width (max effective-width-top effective-width-bot)]
             [layout-top
              (send diag-top lay-out effective-sub-width 'bot 'bot direction)]
             [layout-bot
              (send diag-bot lay-out effective-sub-width 'top 'top direction-bot)]
             [justify-space (max sub-width effective-sub-width)]
             [justify-layout
              (λ (layout)
                (let* ([d (get-field direction layout)]
                       [justify ((justify-content)
                                 (- justify-space (get-field physical-width layout)) 1)]
                       [justify-left (first justify)]
                       [justify-right (second justify)]
                       [strut-left (new hstrut% [direction d] [physical-width justify-left])]
                       [strut-right (new hstrut% [direction d] [physical-width justify-right])])
                  (if (zero? justify-left)
                      (if (zero? justify-right)
                          layout
                          (new happend-layout% [subs (list layout strut-right)] [direction d]))
                      (if (zero? justify-right)
                          (new happend-layout% [subs (list strut-left layout)] [direction d])
                          (new happend-layout% [subs (list strut-left layout strut-right)]
                               [direction d])))))]
             [justified-layouts (map justify-layout (list layout-top layout-bot))])
        (new (if (eq? polarity '+) vappend-block-layout% vappend-forward-backward-layout%)
             [subs justified-layouts] [direction direction]
             [tip-specs `((left . ,left-tip) (right . ,right-tip))])))))

(define station%
  (class block-diagram%
    (init-field terminal? label)
    (super-new [weight (text-width label)])

    (define/override (lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
      ;; requested tips and width don't matter
      (new text-box% [terminal? terminal?] [label label] [direction direction]))))

(define epsilon%
  (class block-diagram%
    (super-new [weight 0])
    (define/override (lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
      (new hstrut% [physical-width width] [direction direction]))))

(define inline-diagram%
  (class diagram% (super-new)))

(define (break-at-helper lst didxs)
  (match didxs
    ['() (list lst)]
    [(cons idx rests) (cons (take lst idx) (break-at-helper (drop lst idx) rests))]))

(define (break-at lst idxs)
  (if (empty? idxs) (list lst)
      (break-at-helper
       lst
       (map (lambda (i prev-i) (- i prev-i)) idxs (cons -1 (drop-right idxs 1))))))

(define sequence%
  (class inline-diagram%
    (init-field subs [min-gap 0] [extra-width-absorb 0.2])
    (unless (> (length subs) 0)
      (raise-arguments-error 'sequence "must sequence at least one diagram"))
    (super-new [weight (+ (apply + (map (λ (s) (get-field weight s)) subs))
                          (* (- (length subs) 1) min-gap))])

    (define/public (enumerate-wraps)
      ;; '((wrap-spec ))
      (let ([sub-wraps (map (lambda (s) (send s enumerate-wraps)) subs)]
            [self-wrap-specs (combinations (range (length subs)))])
        (map
         (lambda (ws) (break-at sub-wraps ws))
         self-wrap-specs)
        #f))

    (define/override (lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
      ;; requested tips don't matter
      #f)))

(define (+map f l) (apply + (map f l)))
(define (display-expr k) (begin (displayln k) k))

(define wrapped-sequence%
  (class sequence% (super-new)
    (init-field wrap-spec)
    (inherit-field subs min-gap extra-width-absorb)
    (define wrapped-subs (break-at subs wrap-spec))

    (define/override (lay-out width [left-tip 'default] [right-tip 'default] [direction 'ltr])
      ;; requested tips don't matter
      ;; TODO: account for extra-width-absorb (0)
      (let*
          ([marker (new ellipsis-marker% [direction direction])]
           [available-width
            (- width (if (empty? wrap-spec) 0 (* 2 (get-field physical-width marker))))]
           [prelim-row-layout-widths
            (map
             (λ (row)
               (let ([total-weight (+map (λ (s) (get-field weight s)) row)]
                     [available-width (- available-width (* min-gap (- (length row) 1)))])
                 (+map (λ (s) (get-field
                               physical-width
                               (send s lay-out
                                     (* available-width (get-field weight s) (/ total-weight))
                                     'default 'default #;TODO direction)))
                       row)))
             wrapped-subs)]
           ; assumption: if (lay-out w1) produces pw1 <= w1,
           ;   then (lay-out w2) for w2 > w1 will produce pw2 <= w2
           ;
           ; maybe also: pw1 <= pw2 (monotonic)
           ; maybe also: (lay-out w) if w <= min-content produces pw = min-content,
           ;   else min-content <= pw <= w
           [effective-width
            (max available-width (apply max prelim-row-layout-widths))]
           [row-layouts
            (map
             (λ (row)
               (let*
                   ([total-weight (+map (λ (s) (get-field weight s)) row)]
                    [available-width (- effective-width (* min-gap (- (length row) 1)))]
                    [sub-layouts
                     ((if (eq? direction 'rtl) reverse identity)
                      (map
                       (λ (s)
                         (send s lay-out
                               (* available-width (get-field weight s) (/ total-weight))
                               'default 'default #;TODO direction))
                       row))]
                    [justify-space
                     (- effective-width (+map (λ (sl) (get-field physical-width sl)) sub-layouts))]
                    [justify-lengths ((justify-content) justify-space (length row) min-gap)]
                    [sub-layouts-and-struts
                     (let loop ([sls sub-layouts] [jls justify-lengths])
                       (if (empty? jls) '()
                           (let ([rests (if (empty? sls) '()
                                            (cons (car sls) (loop (cdr sls) (cdr jls))))])
                             (if (zero? (car jls)) rests
                                 (cons (new hstrut%
                                            [physical-width (car jls)]
                                            [direction direction])
                                       rests)))))])
                 (new happend-layout% [subs sub-layouts-and-struts] [direction direction])))
             wrapped-subs)])
        (new vappend-inline-layout% [subs row-layouts] [direction direction]
             [style 'marker] [marker marker])))))

#;(define happend-inline-diagram%
  (class inline-diagram%
    (init-field diags [min-gap 0] [extra-width-absorb 0.2])
    (unless (> (length diags) 0)
      (raise-arguments-error
       'make-happend-inline-diagram
       "must happend at least one diagram"))
    (unless
        (= 1 (length (remove-duplicates diags eq?
                                        #:key (lambda (d) (get-field direction d)))))
      (raise-arguments-error
       'make-happend-inline-diagram
       "happended diagrams must have same direction"))
    (super-new [natural-width
                (foldl (lambda (diag acc) (+ (get-field natural-width diag) min-gap acc))
                       (get-field natural-width (car diags))
                       (cdr diags))]
               ;; TODO: depends on align-items
               [natural-height
                (apply max (map (lambda (d) (get-field natural-height d)) diags))]
               [num-logical-rows '((left . 1) (right . 1))]
               [direction (get-field direction (car diags))])
    (inherit-field natural-width natural-height)

    (define/override (tip? side spec)
      (case spec
        [(default (logical . 0) top bot)
         (apply max (map (lambda (d) (send d tip? side 'default)) diags))]
        [else #f]))

    (define/override (render left-tip right-tip width)
      (let* ([dc (new record-dc%)]
             [effective-width (max natural-width width)]
             [extra-width (- effective-width natural-width)]
             [sub-extra-width (* extra-width (- 1 extra-width-absorb))]
             [sub-total-natural-width
              (apply + (map (lambda (d) (get-field natural-width d)) diags))]
             [sub-renderings (map (lambda (d)
                                 (send d render
                                       ;; TODO: get different tips per align-items
                                       'default
                                       'default
                                       (* sub-extra-width (/ (get-field natural-width d)
                                                             sub-total-natural-width))))
                               diags)]
             [sub-effective-widths (map (lambda (l) (get-field width l)) sub-renderings)]
             [sub-total-effective-width (apply + sub-effective-widths)]
             [self-extra-width (- effective-width sub-total-effective-width)]
             [self-extra-width-splits
              ((justify-content) self-extra-width (length diags) min-gap)]
             ; TODO: not always left
             [struts-y (tip? 'left 'default)])
        (send dc set-pen (the-strut-pen))
        (set-random-color! dc)
        (foldl
         (lambda (sl extra cum-x)
           (send sl draw! dc cum-x 0)
           (let* ([plus-cum-sl (+ cum-x (get-field width sl))]
                  [plus-cum-sl-extra (+ plus-cum-sl extra)])
             (send dc draw-line plus-cum-sl struts-y plus-cum-sl-extra struts-y)
             plus-cum-sl-extra))
         (let ([first-extra (car self-extra-width-splits)])
           (send dc draw-line 0 struts-y first-extra struts-y)
           first-extra)
         sub-renderings
         (cdr self-extra-width-splits))
        (new rendering% [width effective-width] [height natural-height]
             [draw-proc (send dc get-recorded-procedure)])))))

#;(define (reverse-diagram diag)
  (cond
    [(is-a? diag happend-inline-diagram%)
     (with-destruct-object-as diag
       (diags min-gap extra-width-absorb)
       (new happend-inline-diagram%
            [diags (reverse (map reverse-diagram diag-diags))]
            [min-gap diag-min-gap]
            [extra-width-absorb diag-extra-width-absorb]))]
    [(is-a? diag vappend-block-diagram%)
     (with-destruct-object-as diag
       (diag-top diag-bot gap)
       (new vappend-block-diagram%
            [diag-top (reverse-diagram diag-diag-top)]
            [diag-bot (reverse-diagram diag-diag-bot)]
            [gap diag-gap]))]
    [else diag]))

(define my-svg-dc
  (new svg-dc% [width 500] [height 200] [output "trial.svg"] [exists 'truncate]))
(send my-svg-dc start-doc "")
(send my-svg-dc start-page)

(define my-target (make-bitmap 1000 500))
(define my-bitmap-dc
  (new bitmap-dc% [bitmap my-target]))
(send my-bitmap-dc scale 2 2)

(define expr `(<> - (seq (term "e") (term "a"))
                  (seq (term "ffffffffff") (nonterm "BCD") (term "g"))))
;(define diag (diagram expr))
;(define rendering (send diag render 'default 'default 300))

(define expr-seqseqseq '(seq (seq (seq (term "a")))))
;(define diag-seqseqseq (diagram expr-seqseqseq))
;(define rendering-seqseqseq (send diag-seqseqseq render 'default 'default 400))

(define expr-short '(<> - (<> + (term "c1") (term "c2")) (<> - (seq (<> + (term "a") (term "b")) (term "d")) (term "e"))))
;(define rendering-short (send (diagram expr-short) render '(logical . 2.25) 'default 80))
;(define rendering-short-long (send (diagram expr-short) render '(logical . 2) 'default 200))

;(define my-rendering rendering-short)
;; (displayln (get-field width my-rendering))
;; (send my-rendering draw! my-bitmap-dc 10 10)

(define diag1 (new station% [terminal? #t] [label "hello"]))
(define diag2 (new station% [terminal? #f] [label "bye"]))
(define diag3 (new stack% [diag-top diag1] [diag-bot diag2] [polarity '-]))

(define diag4 (new station% [terminal? #t] [label "Vermont"]))
(define diag5 (new station% [terminal? #f] [label "parametricity"]))
(define diag6 (new stack% [diag-top diag4] [diag-bot diag5] [polarity '+]))

(define diag7 (new stack% [diag-top diag3] [diag-bot diag6] [polarity '+]))
(define layout7 (parameterize ([justify-content flex-end])
                  (send diag7 lay-out 150 '(logical . 1.6) '(logical . 0) 'rtl)))

(define diag8 (new wrapped-sequence% [subs (list diag1 diag2 diag1 diag4 diag5)] [wrap-spec '(0 2)] [min-gap 10]))
(define layout8 (parameterize ([justify-content flex-end])
                  (send diag8 lay-out 300 'default 'default 'rtl)))

(println (get-field physical-width layout8))
(for-each (lambda (cmd) (apply dynamic-send my-bitmap-dc cmd)) (send layout8 render 20 20))
my-target

(send my-svg-dc end-page)
(send my-svg-dc end-doc)
