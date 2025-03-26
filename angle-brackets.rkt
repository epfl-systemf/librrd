#lang racket
(require racket/draw racket/help)
(require (for-syntax racket/syntax))
(provide
 align-items align-items-choices
 ai-baseline ai-top ai-center ai-bottom
 justify-content justify-content-choices
 jc-space-evenly jc-space-between jc-space-around
 jc-start jc-end jc-left jc-center jc-right
 min-strut-width row-gap min-gap flex-absorb
 the-terminal-font terminal-text-width-correction
 the-nonterminal-font nonterminal-text-width-correction
 the-font-size
 the-atom-text-pen the-atom-box-pen the-nonterminal-atom-box-pen the-strut-pen the-atom-box-brush
 arrow-threshold
 layout% text-box% hstrut% happend-layout% ellipsis-marker%
 vappend-inline-layout% vappend-block-layout% vappend-forward-backward-layout%
 diagram% block-diagram% stack%
 inline-diagram% sequence% wrapped-sequence% station% epsilon%
 distribute-fun distribute-linear distribute-quadratic distribute-extreme
 desugar diagram local-wraps-< local-wraps-alt-<
 figure-margin print-layout-pdf! print-layout-svg!)


(define (display-expr k) (begin (displayln k) k))

(define (~= x y) (> 0.0001 (abs (- x y))))

(define (linear-interpolate xstart ystart xend yend x)
  (+ ystart (* (/ (- x xstart) (- xend xstart)) (- yend ystart))))

(define (pre-post-pend pre mid post)
  (append
   (if pre (if (list? pre) pre (list pre)) '())
   (if (list? mid) mid (list mid))
   (if post (if (list? post) post (list post)) '())))

;; types of align-items
(define ai-baseline '((name . baseline) (value logical . 0)))
(define ai-top '((name . top) (value physical . 0)))
(define ai-center '((name . center) (value physical . 0.5)))
(define ai-bottom '((name . bottom) (value physical . 1)))

(define align-items-choices
  `(("top" . ,ai-top)
    ("center" . ,ai-center)
    ("bottom" . ,ai-bottom)
    ("baseline" . ,ai-baseline)))

(define (directional-reverse direction l)
  (unless (memq direction '(ltr rtl)) (raise-argument-error 'direction "direction?" direction))
  ((if (eq? direction 'rtl) reverse identity) l))

;; a type of justify-content
; contract: must not exceed total-space
; contract: total-space must be at least (* (min-gap) num-subs)
(define (jc-space-evenly total-space num-subs #;direction _)
  (let* ([each-basis (/ total-space (+ num-subs 1))]
         [each-mid-space (max (min-gap) each-basis)]
         [each-end-space (/ (- total-space (* each-mid-space (- num-subs 1))) 2)])
    (pre-post-pend each-end-space (build-list (- num-subs 1) (λ _ each-mid-space)) each-end-space)))

(define (jc-space-between total-space num-subs #;direction _)
  (case num-subs
    [(1) (jc-center total-space num-subs _)]
    [(2) (list 0 total-space 0)]
    [else
     (pre-post-pend 0 (jc-space-evenly total-space (- num-subs 2) _) 0)]))

(define (jc-space-around total-space num-subs #;direction _)
  (let* ([each-basis (/ total-space num-subs)]
         [each-mid-space (max (min-gap) each-basis)]
         [each-end-space (/ (- total-space (* each-mid-space (- num-subs 1))) 2)])
    (pre-post-pend each-end-space (build-list (- num-subs 1) (λ _ each-mid-space)) each-end-space)))

(define (jc-right total-space num-subs #;direction _)
  (pre-post-pend
   (- total-space (* (min-gap) (- num-subs 1)))
   (build-list (- num-subs 1) (λ _ (min-gap)))
   0))

(define (jc-end total-space num-subs direction)
  (directional-reverse direction (jc-right total-space num-subs direction)))

(define (jc-left total-space num-subs #;direction _)
  (pre-post-pend
   0
   (build-list (- num-subs 1) (λ _ (min-gap)))
   (- total-space (* (min-gap) (- num-subs 1)))))

(define (jc-start total-space num-subs direction)
  (directional-reverse direction (jc-left total-space num-subs direction)))

(define (jc-center total-space num-subs #;direction _)
  (let ([each-end-space (/ (- total-space (* (min-gap) (- num-subs 1))) 2)])
    (pre-post-pend
     each-end-space
     (build-list (- num-subs 1) (λ _ (max (min-gap) 0)))
     each-end-space)))

(define justify-content-choices
  `(("space-evenly" . ,jc-space-evenly)
    ("space-between" . ,jc-space-between)
    ("space-around" . ,jc-space-around)
    ("start" . ,jc-start)
    ("end" . ,jc-end)
    ("left" . ,jc-left)
    ("center" . ,jc-center)
    ("right" . ,jc-right)))

;; affect layout
(define align-items (make-parameter ai-top))
(define justify-content (make-parameter jc-space-evenly))

(define the-terminal-font
  (make-parameter (make-font #:font-list the-font-list
                             #:size 12
                             #:face "Ubuntu Mono"
                             #:family 'modern
                             #:style 'normal
                             #:weight 'normal)))
(define the-nonterminal-font
  (make-parameter (the-terminal-font)))
(define the-font-size
  (make-derived-parameter
   the-terminal-font
   (lambda (fs) (make-font #:font-list the-font-list
                           #:size fs
                           #:face (send (the-terminal-font) get-face)
                           #:family (send (the-terminal-font) get-family)
                           #:style (send (the-terminal-font) get-style)
                           #:weight (send (the-terminal-font) get-weight)))
   (lambda (f) (send f get-size))))

(define (min-strut-width) (* (+ (the-font-size) 12) 1/4))
(define (row-gap) 8)

(define text-measurement-dc (new svg-dc% [width 1000] [height 1000] [output (open-output-nowhere)]))
(define terminal-text-width-correction (make-parameter identity))
(define nonterminal-text-width-correction (make-parameter identity))

(define (text-width text terminal?)
  (send text-measurement-dc set-font (if terminal? (the-terminal-font) (the-nonterminal-font)))
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    ((if terminal? (terminal-text-width-correction) (nonterminal-text-width-correction)) width)))

(define (text-height text terminal?)
  (send text-measurement-dc set-font (if terminal? (the-terminal-font) (the-nonterminal-font)))
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    height))

(define min-gap (make-parameter 0))
(define flex-absorb (make-parameter 0.0))

;; affect rendering
(define the-atom-text-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-atom-box-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-nonterminal-atom-box-pen
  (make-parameter (the-atom-box-pen)))
(define the-strut-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-atom-box-brush
  (make-parameter (make-brush #:style 'transparent)))

(define arrow-threshold (make-parameter 5))


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
        [(side) (cddr (assq side tips))]
        [(side spec) (inner #f tip-y side spec)]))
    (abstract render #;(render x y))))

(define (vertical? tip-spec) (eq? tip-spec 'vertical))

(define (direction-toggle d)
  (case d [(ltr) 'rtl] [(rtl) 'ltr] [else (raise-argument-error 'd "direction?" d)]))

(define vappend-block-layout%
  (class layout%
    (init-field tip-specs subs)

    (define/public (side-struts? side)
      (if (not (vertical? (cdr (assq side tip-specs)))) (* 3/2 (min-strut-width)) 0))

    (define ((get-rows side) sub)
      (if (and (is-a? sub vappend-block-layout%)
               (not (vertical? (cadr (assq side (get-field tips sub))))))
          1
          (cdr (assq side (get-field num-rows sub)))))
    (init [num-rows
           (for/list ([side '(left right)])
             (cons side (apply + (map (get-rows side) subs))))])
    (define init-num-rows num-rows)

    (define/augride (tip-y side spec)
      (match spec
        ; is this one really needed?
        ['vertical (send (first subs) tip-y side spec)]
        [(cons 'logical (? number? row-num))
         (let* ([num-rows (- (cdr (assq side init-num-rows)) 1)])
           (unless (<= 0 row-num num-rows)
             (raise-arguments-error
              'vappend-block-layout-tip-y
              "logical tip spec must be in [0, R-1]"
              "height" row-num "L" num-rows))
           (let loop ([n row-num] [cumul-y (- (/ (row-gap) 2))] [subs subs])
             (let* ([sub (first subs)]
                    [sub-rows (- ((get-rows side) sub) 1)]
                    [sub-top-y (+ cumul-y (/ (row-gap) 2))]
                    [next-cumul-y
                     (+ sub-top-y (get-field physical-height sub) (/ (row-gap) 2))])
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
                           (quotient (- (cdr (assq side init-num-rows)) 1) 2)))]
        [(cons 'physical (? number? row-num))
         (unless (<= 0 row-num 1)
           (raise-arguments-error
            'vappend-block-layout-tip-y
            "physical tip spec must be in [0, 1]"
            "height" row-num))
         (linear-interpolate
          0 (send (first subs) tip-y side '(physical . 0))
          1 (let ([last-sub (last subs)])
              (+ (apply + (map (lambda (s) (get-field physical-height s)) subs))
                 (* (- (length subs) 1) (row-gap))
                 (- (get-field physical-height last-sub))
                 (send last-sub tip-y side '(physical . 1))))
          row-num)]
        [else #f]))

    (let ([sub-widths (map (lambda (s) (get-field physical-width s)) subs)])
      ; inexact equality
      (unless (~= (apply max sub-widths) (apply min sub-widths))
        (raise-arguments-error 'vappend-layout "subs must have equal widths"
                               "sub-widths" sub-widths))
      (super-new
       [physical-width (+ (first sub-widths) (side-struts? 'left) (side-struts? 'right))]
       [physical-height
        (+ (apply + (map (lambda (s) (get-field physical-height s)) subs))
           (* (- (length subs) 1) (row-gap)))]
       [num-rows init-num-rows]
       [tips (map (lambda (s)
                    (let ([side (car s)] [spec (cdr s)])
                      (cons side (cons spec (tip-y side spec)))))
                  tip-specs)]))

    (inherit-field direction tips physical-width)

    (define/public (check-directions)
      (unless (andmap (lambda (s) (eq? (get-field direction s) direction)) subs)
        (raise-arguments-error
         'vappend-layout
         "subs must all have same direction as this layout"
         "subs" subs "direction" direction)))
    (check-directions)

    (define/overment (render x y)
      (let ([sub-x (+ x (side-struts? 'left))]
            [arc-size (* 2 (min-strut-width))])
        (let-values
            ([(sub-renders sub-ys)
              (for/fold
                  ([sub-y y]
                   [sub-renders '()]
                   [sub-ys '()]
                   #:result (values (apply append sub-renders)
                                    (reverse sub-ys)))
                  ([sub subs])
                (values (+ sub-y (get-field physical-height sub) (row-gap))
                        (cons (send sub render sub-x sub-y) sub-renders)
                        (cons sub-y sub-ys)))])
          (apply
           append
           sub-renders
           `((set-pen ,(the-strut-pen)))
           (for/list ([side '(left right)]
                      [bracket-x (list sub-x (+ sub-x (get-field physical-width (first subs))))]
                      [inner-dx (list (/ arc-size 2) (- (/ arc-size 2)))]
                      [inner-arc-dx (list 0 (- arc-size))]
                      [outer-arc-dx (list (- arc-size) 0)]
                      [positive-theta (list (/ pi 2) 0)]
                      [negative-theta (list 0 (/ pi 2))])
             (let* ([self-tip-y (+ y (tip-y side))]
                    [sub-tip-ys (map (λ (r) (+ y (tip-y side `(logical . ,r))))
                                     (range (cdr (assq side init-num-rows))))]
                    [arc (λ (dx dy t) (λ (y) `(draw-arc ,(+ bracket-x dx) ,(+ y dy)
                                                        ,arc-size ,arc-size
                                                        ,t ,(+ t (/ pi 2)))))]
                    [inner-up-arc (arc inner-arc-dx (- arc-size) (+ negative-theta pi))]
                    [outer-up-arc (arc outer-arc-dx (- arc-size) (+ positive-theta pi))]
                    [outer-down-arc (arc outer-arc-dx 0 negative-theta)]
                    [inner-down-arc (arc inner-arc-dx 0 positive-theta)]
                    [vline (λ (sy ey) `(draw-line ,bracket-x ,(+ sy (/ arc-size 2))
                                                  ,bracket-x ,(- ey (/ arc-size 2))))])
               (inner
                (if (vertical? (cdr (assq side tip-specs))) '()
                    ; only top-level vappend draws curves and connecting struts
                    (for/fold ([cmds '()]
                               [vline-above? #f]
                               [vline-below? #f]
                               #:result (pre-post-pend
                                         (and vline-above?
                                              (list (outer-up-arc self-tip-y)
                                                    (vline (first sub-tip-ys) self-tip-y)))
                                         (cons
                                          `(draw-line ,(- bracket-x inner-dx) ,self-tip-y
                                                      ,(- bracket-x (* 3/2 inner-dx)) ,self-tip-y)
                                          cmds)
                                         (and vline-below?
                                              (list (outer-down-arc self-tip-y)
                                                    (vline self-tip-y (last sub-tip-ys))))))
                              ([ty sub-tip-ys])
                      (cond
                        [(~= ty self-tip-y)
                         (values
                          (cons `(draw-line ,(- bracket-x inner-dx) ,ty ,(+ bracket-x inner-dx) ,ty)
                                cmds)
                          vline-above? vline-below?)]
                        [(< ty self-tip-y)
                         (values (cons (inner-down-arc ty) cmds) #t vline-below?)]
                        [(> ty self-tip-y)
                         (values (cons (inner-up-arc ty) cmds) vline-above? #t)])))
                render self-tip-y sub-ys side bracket-x inner-dx vline
                arc-size inner-arc-dx outer-arc-dx positive-theta negative-theta
                inner-up-arc outer-up-arc outer-down-arc inner-down-arc)))))))))

(define vappend-forward-backward-layout%
  (class vappend-block-layout%
    (init ([internal-subs subs]))
    (unless (= (length internal-subs) 2)
      (raise-arguments-error
       'vappend-forward-backward-layout
       "vappend-forward-backward must have exactly 2 subs"
       "subs" internal-subs))

    (init [(init-tip-specs tip-specs) '((left . default) (right . default))])
    (define tip-specs init-tip-specs)

    (define/override (tip-y side spec)
      (case spec
        [(default vertical) (super tip-y side '(logical . 0))]
        [else (super tip-y side spec)]))

    (define/override (side-struts? side)
      (case (cdr (assq side tip-specs))
        [(vertical default (logical . 0) (physical . 0)) (min-strut-width)]
        [else (displayln "are we ever here") (* 5/2 (min-strut-width))]))

    (super-new [subs internal-subs] [num-rows '((left . 1) (right . 1))]
               [tip-specs tip-specs])

    (inherit-field direction subs num-rows)
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
         "second sub" (second subs) "direction" direction)))

    (define/augride (render self-tip-y sub-ys side bracket-x inner-dx vline
                            arc-size inner-arc-dx outer-arc-dx positive-theta negative-theta
                            inner-up-arc outer-up-arc outer-down-arc inner-down-arc)
      (let* ([bot-sub (second subs)]
             [sub-tip-ys
              (map (λ (r) (+ (second sub-ys) (send bot-sub tip-y side `(logical . ,r))))
                   (range (cdr (assq side (get-field num-rows (second subs))))))]
             [top-tip-y (+ (first sub-ys) (send (first subs) tip-y side))]
             [bot-tip-y (last sub-tip-ys)])
        (append
         ; for the top sub
         (list (inner-down-arc top-tip-y))
         ; for all subs except top
         (append-map (λ (ty) (list (inner-up-arc ty))) sub-tip-ys)
         ; for the self-tip
         (cond
           [(~= self-tip-y top-tip-y)
            `((draw-line ,(- bracket-x (if (eq? (cdr (assq side tip-specs)) 'vertical) 0 inner-dx))
                         ,self-tip-y
                         ,(+ bracket-x inner-dx) ,self-tip-y))]
           [(< self-tip-y top-tip-y)
            (list (outer-down-arc self-tip-y)
                  (vline self-tip-y top-tip-y)
                  (inner-up-arc top-tip-y)
                  `(draw-line ,(- bracket-x inner-dx) ,self-tip-y
                              ,(- bracket-x (* 3/2 inner-dx)) ,self-tip-y))]
           [else
            (list (outer-up-arc self-tip-y)
                  `(draw-line ,(- bracket-x inner-dx) ,self-tip-y
                              ,(- bracket-x (* 3/2 inner-dx)) ,self-tip-y))])
         ; unconditional vline
         (list (vline top-tip-y bot-tip-y)))))))

(define vappend-inline-layout%
  (class vappend-block-layout%
    (init [(init-subs subs)])
    (init-field style [marker #f])
    (init [(init-direction direction) 'ltr]
          [tip-specs '((left . default) (right . default))])

    (define-values (start-side end-side)
      (apply values (directional-reverse init-direction '(left right))))

    (define side-finishing?
      (list (cons start-side (match (cdr (assq start-side tip-specs))
                               [(cons 'physical r) (not (= r 0))]
                               [else #f]))
            (cons end-side (match (cdr (assq end-side tip-specs))
                             [(cons 'physical r) (not (= r 1))]
                             [else #f]))))

    (define/override (side-struts? side)
      (if (cdr (assq side side-finishing?)) (* 3/2 (min-strut-width)) 0))

    (define subs
      (case style
        [(marker)
         (unless (is-a? marker inline-layout%)
           (raise-arguments-error
            'vappend-inline-layout
            "when style is 'marker, marker must be an inline-layout%"
            "marker" marker))
         (let ([marker-width (get-field physical-width marker)])
           (unless (> marker-width (min-strut-width))
             (raise-arguments-error
              'vappend-inline-layout
              "marker width must be at least min-strut-width"
              "marker-width" marker-width))
           (if (= (length init-subs) 1) init-subs
               (let*-values ([(first-subs rest-subs) (split-at init-subs 1)]
                             [(mid-subs last-subs) (split-at-right rest-subs 1)])
                 (let ([struct-sub-markers
                        (for/list ([sub (list (first first-subs) (first last-subs))]
                                   [maybe-reverse (if (eq? init-direction 'rtl)
                                                      (list reverse identity)
                                                      (list identity reverse))]
                                   [tip-side (list start-side end-side)])
                          (new happend-layout% [fuse? #t] [direction init-direction]
                               [subs
                                (maybe-reverse
                                 (append
                                  (if (or (vertical? (cdr (assq tip-side tip-specs)))
                                          (not (= 0 (side-struts? tip-side))))
                                      (list (new hspace% [direction init-direction])
                                            (new hstrut%
                                                 [physical-width (- marker-width (min-strut-width))]
                                                 [direction init-direction] [always-arrow #t]))
                                      (list (new hstrut% [physical-width marker-width]
                                                 [direction init-direction] [always-arrow #t])))
                                  (list sub marker)))]))])
                   (pre-post-pend
                    (first struct-sub-markers)
                    (map (λ (s) (new happend-layout% [direction init-direction]
                                     [subs (list marker s marker)]))
                         mid-subs)
                    (second struct-sub-markers))))))]

        [(boustrophedon)
         (unless (odd? (length init-subs))
           (raise-arguments-error
            'vappend-inline-layout
            "when style is 'boustrophedon, must have odd number of subs"
            "subs" init-subs))
         init-subs]

        [(bare) init-subs]

        [else
         (raise-arguments-error
          'vappend-inline-layout "style must be 'bare, 'marker, or 'boustrophedon"
          "style" style)]))

    (define/override (tip-y side spec)
      (match spec
        [(or 'default 'vertical) (tip-y side '(logical . 0))]
        [(cons 'physical (? number? row-num))
         (unless (<= 0 row-num 1)
           (raise-arguments-error
            'vappend-inline-layout-tip-y
            "physical tip spec must be in [0, 1]"
            "height" row-num))
         (linear-interpolate
          0 (tip-y start-side '(logical . 0))
          1 (tip-y end-side '(logical . 0))
          row-num)]
        [(cons 'logical 0)
         (if (eq? side start-side)
             (send (first subs) tip-y side)
             (let-values ([(nonlast-subs last-sub) (split-at-right subs 1)])
               (+ (send (first last-sub) tip-y side)
                  (+map (λ (s) (get-field physical-height s)) nonlast-subs)
                  (* (length nonlast-subs) (row-gap)))))]
        [else (super tip-y side spec)]))

    (super-new
     [subs subs] [direction init-direction]
     [num-rows '((left . 1) (right . 1))]
     [tip-specs tip-specs])

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

    (define/augride (render self-tip-y sub-ys side bracket-x inner-dx vline
                            arc-size inner-arc-dx outer-arc-dx positive-theta negative-theta
                            inner-up-arc outer-up-arc outer-down-arc inner-down-arc)
      (when (eq? style 'boustrophedon)
        (raise-user-error "boustrophedon layout not implemented yet"))
      (if (cdr (assq side side-finishing?))
          (let ([finish-y (+ self-tip-y (- (tip-y side)) (tip-y side '(logical . 0)))]
                [finish-strut `(draw-line ,(- bracket-x inner-dx) ,self-tip-y
                                          ,(- bracket-x (* 3/2 inner-dx)) ,self-tip-y)])
            (if (< finish-y self-tip-y)
                (list (inner-down-arc finish-y)
                      (vline finish-y self-tip-y)
                      (outer-up-arc self-tip-y)
                      finish-strut)
                (list (outer-down-arc self-tip-y)
                      (vline self-tip-y finish-y)
                      (inner-up-arc finish-y)
                      finish-strut)))
          '()))))

(define inline-layout%
  (class layout%
    (init [num-rows '((left . 1) (right . 1))])
    (super-new [num-rows num-rows])
    (define/augment (tip-y side spec)
      (or (inner #f tip-y side spec)
          (match spec
            [(or 'default '(logical . 0) 'vertical (cons 'physical _)) (tip-y side)]
            [else #f])))))

(define hspace%
  (class inline-layout%
    (init [(init-width physical-width) (min-strut-width)])
    (super-new [physical-height 0]
               [tips '((left default . 0) (right default . 0))]
               [physical-width init-width] #;[direction pass-through])
    (define/override (render x y) '())))

(define hstrut%
  (class inline-layout%
    (init-field [always-arrow #f])
    (define base-diff (* (the-font-size) 0.125))
    (define y-diff (* base-diff 2))
    (super-new [physical-height (* 2 y-diff)]
               [tips `((left default . ,y-diff) (right default . ,y-diff))]
               #;[physical-width pass-through] #;[direction pass-through])
    (inherit-field physical-width direction)

    (define/override (render x y)
      (append
       `((set-pen ,(the-strut-pen))
         (draw-line ,x ,(+ y y-diff) ,(+ x physical-width) ,(+ y y-diff)))
       (if (or (and always-arrow (>= physical-width (* 6 base-diff)))
               (>= physical-width (* (min-strut-width) (arrow-threshold))))
           (let ([x-diff ((if (eq? direction 'ltr) + -) (* base-diff 3))])
             `((draw-lines
                ((,(- x-diff) . ,(- y-diff)) (,x-diff . 0) (,(- x-diff) . ,y-diff))
                                           ; optical correction
                ,(+ x (/ physical-width 2) (* x-diff 0.3)) ,(+ y y-diff))))
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
    (define label-width (text-width label terminal?))
    (define label-height (text-height label terminal?))
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
             [lstrut-rx (+ lstrut-lx (min-strut-width))]
             [rstrut-lx (+ x (min-strut-width) box-width)]
             [rstrut-rx (+ rstrut-lx (min-strut-width))])
        `((set-pen ,(the-atom-text-pen))
          (set-font ,(if terminal? (the-terminal-font) (the-nonterminal-font)))
          (draw-text ,label ,text-x ,text-y #t)
          (set-pen ,(if terminal? (the-atom-box-pen) (the-nonterminal-atom-box-pen)))
          (set-brush ,(the-atom-box-brush))
          (,(if terminal? 'draw-rounded-rectangle 'draw-rectangle)
           ,box-x ,box-y ,box-width ,box-height))))))

(define text-marker%
  (class text-box%
    (super-new [terminal? #f] [padding-x (min-strut-width)])
    (init-field [y-alignment-magic (* (the-font-size) 0.23)])
    (inherit-field label padding-x)
    (define/override (render x y)
      (let* ([box-x x]
             [box-y y]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y y-alignment-magic)])
        `((set-pen ,(the-atom-text-pen))
          (set-font ,(the-terminal-font))
          (draw-text ,label ,text-x ,text-y #t))))))

(define ellipsis-marker%
  (class text-marker%
    (super-new [label "…"] [y-alignment-magic (* (the-font-size) 0.08)])))

(define happend-layout%
  (class inline-layout%
    (init [(init-subs subs)] [fuse? #f])
    (field
     [subs
      (if fuse?
          (let* ([spliced-subs
                  (apply append
                         (map (λ (s) (if (is-a? s happend-layout%) (get-field subs s) (list s)))
                              init-subs))]
                 ;; TODO: remove the need for these checks
                 [space? (λ (s) (and (is-a? s hspace%) s))]
                 [start-space (let ([ss (dropf spliced-subs (is-a?/c hstrut%))])
                                (and (not (empty? ss)) (space? (first ss))))]
                 [end-space (let ([ss (dropf-right spliced-subs (is-a?/c hstrut%))])
                              (and (not (empty? ss)) (space? (last ss))))]
                 [spliced-subs (filter-not space? spliced-subs)])
            (for/fold ([cur-hstrut #f]
                       [subs '()]
                       #:result (pre-post-pend start-space
                                               (reverse (if cur-hstrut (cons cur-hstrut subs) subs))
                                               end-space))
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
      (vertical? (cadr (assq 'left (get-field tips (first subs))))))
    (define expose-last-right-tips?
      (vertical? (cadr (assq 'right (get-field tips (last subs))))))

    (unless (>= (length subs) 1)
      (raise-arguments-error
       'happend-layout
       "happend-layout must have at least one sub layout"))

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

    (define/augride (tip-y side spec)
      (match spec
        [(cons 'logical _)
         (cond
           [(and (eq? side 'left) expose-first-left-tips?)
            (+ (first sub-ys) (send (first subs) tip-y side spec))]
           [(and (eq? side 'right) expose-last-right-tips?)
            (+ (last sub-ys) (send (last subs) tip-y side spec))]
           [else #f])]
        [else #f]))

    (super-new
     [physical-width width]
     [physical-height height]
     [tips `((left default . ,left-tip) (right default . ,right-tip))]
     [num-rows `((left . ,(if expose-first-left-tips?
                              (cdr (assq 'left (get-field num-rows (first subs))))
                              1))
                 (right . ,(if expose-last-right-tips?
                               (cdr (assq 'right (get-field num-rows (last subs))))
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
    (init-field flex)

    (define/pubment (min-content [start-tip (cdr (assq 'value (align-items)))]
                                 [end-tip (cdr (assq 'value (align-items)))]
                                 [direction 'ltr])
      (inner (raise-user-error "min-content not implemented!")
             min-content start-tip end-tip direction))
    (define/pubment (max-content [start-tip (cdr (assq 'value (align-items)))]
                                 [end-tip (cdr (assq 'value (align-items)))]
                                 [direction 'ltr])
      (inner (raise-user-error "max-content not implemented!")
             max-content start-tip end-tip direction))

    ;; Attempt to lay out the diagram with the requested tips and width. Will
    ;; always succeed, but the resulting tips and widths may not be what were
    ;; requested, even if it would have been possible.
    ;
    ; to SVG? HTML? TiKZ? how to have different backends?
    (define/pubment (lay-out width
                             [start-tip (cdr (assq 'value (align-items)))]
                             [end-tip (cdr (assq 'value (align-items)))]
                             [direction 'ltr]
                             [depth 0])
      (inner (raise-user-error "lay-out not implemented!")
             lay-out width start-tip end-tip direction depth))))

(define block-diagram%
  (class diagram% (super-new)))

(define (justify-layouts-without-zero-hstruts justify-space layouts direction)
  (let* ([justify-lengths ((justify-content) justify-space (length layouts) direction)]
         [layouts-and-struts
          (let loop ([los layouts] [jls justify-lengths])
            (if (empty? jls) '()
                (let ([rests (if (empty? los) '()
                                 (cons (car los) (loop (cdr los) (cdr jls))))])
                  (if (~= 0 (car jls)) rests
                      (cons (new hstrut% [physical-width (car jls)] [direction direction])
                            rests)))))])
    (if (= 1 (length layouts-and-struts))
        (first layouts-and-struts) ; only one layout, and completely fills space
        (new happend-layout% [subs layouts-and-struts] [fuse? #t] [direction direction]))))

(define stack%
  (class block-diagram%
    (init-field diag-top diag-bot polarity)
    (init [(init-flex flex) #t] [wrapped? #f])
    (unless (memq polarity '(+ -))
      (raise-arguments-error 'stack "polarity must be '+ or '-" "polarity" polarity))

    (super-new [flex init-flex])

    (define (maybe-tips-width start-tip end-tip)
      (let ([start-tip (if (and (eq? (cdr (assq 'name (align-items))) 'baseline)
                                (eq? polarity '+)
                                (is-a? diag-top epsilon%)
                                (not (vertical? start-tip)))
                           '(logical . 1)
                           start-tip)]
            [end-tip (if (and (eq? (cdr (assq 'name (align-items))) 'baseline)
                              (eq? polarity '+)
                              (is-a? diag-top epsilon%)
                              (not (vertical? end-tip)))
                         '(logical . 1)
                         end-tip)])
        (if (eq? polarity '-)
            (for/sum ([tip (list start-tip end-tip)])
              (case tip
                [(vertical default (logical . 0) (physical . 0)) (min-strut-width)]
                [else (* 5/2 (min-strut-width))]))
            (for/sum ([tip (list start-tip end-tip)])
              (match tip
                [(or 'default (cons 'logical _) (cons 'physical _)) (* 3/2 (min-strut-width))]
                [else 0])))))

    (define (-content which start-tip end-tip direction)
      (+ (max (dynamic-send diag-top which 'vertical 'vertical direction)
              (dynamic-send diag-bot which 'vertical 'vertical direction))
         (maybe-tips-width start-tip end-tip)))

    (define/augride (min-content start-tip end-tip direction)
      (-content 'min-content start-tip end-tip direction))
    (define/augride (max-content start-tip end-tip direction)
      (-content 'max-content start-tip end-tip direction))

    (inherit-field flex)

    (field
     [height
      (if wrapped?
          (get-field physical-height (lay-out (max-content 'default 'default 'ltr)))
          'undefined)])

    #;(field
     [global-wraps-measures
      (if wrapped?
          'undefined
          (for*/list ([top-wrap (get-field global-wraps-measures diag-top)]
                      [bot-wrap (get-field global-wraps-measures diag-bot)])
            (let ([wrapped (new stack% [diag-top (cdr (assq 'wrap top-wrap))]
                                [diag-bot (cdr (assq 'wrap bot-wrap))]
                                [polarity polarity] [flex flex] [wrapped? #t])])
              (list
               (cons 'natural-width (send wrapped min-content 'default 'default 'ltr))
               (cons 'height (get-field height wrapped))
               (cons 'wrap-specs (cons '() (map (λ (wm) (cdr (assq 'wrap-specs wm))) (list top-wrap bot-wrap))))
               (cons 'wrap wrapped)))))])

    (define/augride (lay-out width start-tip end-tip direction depth)
      (let ([start-tip (if (and (eq? (cdr (assq 'name (align-items))) 'baseline)
                                (eq? polarity '+)
                                (is-a? diag-top epsilon%)
                                (not (vertical? start-tip)))
                           '(logical . 1)
                           start-tip)]
            [end-tip (if (and (eq? (cdr (assq 'name (align-items))) 'baseline)
                              (eq? polarity '+)
                              (is-a? diag-top epsilon%)
                              (not (vertical? end-tip)))
                         '(logical . 1)
                         end-tip)])
        (let* ([clamped-width
                ((if flex (λ (a b) b) min)
                 (max-content start-tip end-tip direction)
                 (max (min-content start-tip end-tip direction) width))]
               [clamped-available-width (- clamped-width (maybe-tips-width start-tip end-tip))]
               [layout-top (send diag-top lay-out clamped-available-width 'vertical 'vertical direction (+ 1 depth))]
               [layout-bot
                (send diag-bot lay-out clamped-available-width 'vertical 'vertical
                      (if (eq? polarity '+) direction (direction-toggle direction))
                      (+ 1 depth))]
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
               [tip-specs (map cons '(left right)
                               (directional-reverse direction `(,start-tip ,end-tip)))]))))))

(define inline-diagram%
  (class diagram% (super-new)))

(define atomic-inline-diagram%
  (class inline-diagram% (super-new)
    (define (vertical-tip-space-width start-tip end-tip)
      (+ (if (vertical? start-tip) (min-strut-width) 0)
         (if (vertical? end-tip) (min-strut-width) 0)))
    (define/augment (min-content start-tip end-tip _ #;direction)
      (+ (inner 0 min-content start-tip end-tip) (vertical-tip-space-width start-tip end-tip)))
    (define/augment (max-content start-tip end-tip _ #;direction)
      (+ (inner 0 max-content start-tip end-tip) (vertical-tip-space-width start-tip end-tip)))
    (define/augment (lay-out width start-tip end-tip direction _ #;depth)
      (let ([tip-space
             (λ (tip) (and (vertical? tip) (new hspace% [direction direction])))])
        (new happend-layout%
             [subs (directional-reverse
                    direction
                    (pre-post-pend
                     (tip-space start-tip)
                     (inner #f lay-out (- width (vertical-tip-space-width start-tip end-tip))
                            start-tip end-tip direction)
                     (tip-space end-tip)))]
             [fuse? #t] [direction direction])))))

(define station%
  (class atomic-inline-diagram%
    (super-new [flex #f])
    (init-field terminal? label)
    (define (init-text-box) (new text-box% [terminal? terminal?] [label label]))
    (define (init-layout-width)
      (+ (min-strut-width) (get-field physical-width (init-text-box))))
    (field [height (get-field physical-height (init-text-box))])
    (field [global-wraps-measures
            (list (list (cons 'natural-width (init-layout-width))
                        (cons 'height height)
                        (cons 'wrap-specs '(() . ()))
                        (cons 'wrap this)))])
    (define/augride (min-content _ #;start-tip __ #;end-tip) (init-layout-width))
    (define/augride (max-content _ #;start-tip __ #;end-tip) (init-layout-width))

    (define/augride (lay-out _ #;width start-tip end-tip direction)
      (let ([tip-strut (new hstrut% [physical-width (/ (min-strut-width) 2)] [direction direction])]
            [text-box (new text-box% [terminal? terminal?] [label label] [direction direction])])
        (new happend-layout% [subs (list tip-strut text-box tip-strut)] [direction direction])))))

(define epsilon%
  (class atomic-inline-diagram%
    (super-new [flex #t])
    (field [height 0])
    (field [global-wraps-measures
            (list (list (cons 'natural-width 0)
                        (cons 'height height)
                        (cons 'wrap-specs '(() . ()))
                        (cons 'wrap this)))])
    (define/augride (lay-out width _ #;start-tip __ #;end-tip direction)
      (new hstrut% [physical-width width] [direction direction]))))

(define ellipsis%
  (class atomic-inline-diagram%
    (super-new [flex #f])
    (define init-ellipsis (new ellipsis-marker%))
    (define (init-layout-width)
      (+ (min-strut-width) (get-field physical-width init-ellipsis)))
    (field [height (get-field physical-height init-ellipsis)])
    (field [global-wraps-measures
            (list (list (cons 'natural-width init-layout-width)
                        (cons 'height height)
                        (cons 'wrap-specs '(() . ()))
                        (cons 'wrap this)))])
    (define/augride (min-content _ #;start-tip __ #;end-tip) (init-layout-width))
    (define/augride (max-content _ #;start-tip __ #;end-tip) (init-layout-width))

    (define/augride (lay-out _ #;width start-tip end-tip direction)
      (let ([tip-strut (new hstrut% [physical-width (/ (min-strut-width) 2)] [direction direction])]
            [marker (new ellipsis-marker% [direction direction])])
        (new happend-layout% [subs (list tip-strut marker tip-strut)] [direction direction])))))

(define (break-at lst idxs)
  (if (empty? idxs)
      (list lst)
      (let helper ([lst lst]
                   [didxs (map (λ (i prev-i) (- i prev-i)) idxs (cons -1 (drop-right idxs 1)))])
        (match didxs
          ['() (list lst)]
          [(cons idx rests) (cons (take lst idx) (helper (drop lst idx) rests))]))))

(define (wrap-specs-badness wrap-specs depth-f)
  (let rec ([ws wrap-specs] [depth 0])
    (let ([self-ws (first ws)] [subs-ws (rest ws)])
      (apply + (* (depth-f depth) (length self-ws)) (map (λ (sws) (rec sws (+ 1 depth))) subs-ws)))))

(define ((make-lexicographic-< . accessors) x1 x2)
  (let loop ([accs accessors])
    (and (not (empty? accs))
         (let ([acc1 ((car accs) x1)] [acc2 ((car accs) x2)])
           (or (and (~= acc1 acc2)
                    (loop (cdr accs)))
               (< acc1 acc2))))))

#;(define global-wraps-<
  (make-lexicographic-<
   (λ (w) (cdr (assq 'height w)))
   (λ (w) (wrap-specs-badness (cdr (assq 'wrap-specs w))))
   (λ (w) (cdr (assq 'natural-width w)))))

(define (global-wraps-< min-content-wrap max-content-wrap)
  (let* (#;[max-content-height (get-field height max-content-wrap)]
         #;[min-content-height (cdr (assq 'height min-content-wrap))]
         #;[height (λ (gw) (/ (cdr (assq 'height gw)) min-content-height))]
         #;[max-content-width (send max-content-wrap min-content 'default 'default 'ltr)]
         [natural-width (λ (gw) (cdr (assq 'natural-width gw)))]
         #;[min-content-badness
          (wrap-specs-badness (cdr (assq 'wrap-specs min-content-wrap))
                              (λ (d) (expt 2 d)))]
         [wrap-badness
          (λ (gw) (wrap-specs-badness (cdr (assq 'wrap-specs gw))
                                      (λ (d) (+ d 1))))]
         #;[badness (λ (gw) (+map (λ (fn wt) (* wt (fn gw)))
                                (list height natural-width wrap-badness) '(1 1 1)))])
    (make-lexicographic-<
     wrap-badness
     natural-width)
    #;(λ (gw1 gw2) (< (badness gw1) (badness gw2)))))

(define local-wraps-<
  (make-parameter
   (λ (start-tip end-tip direction _ __ ___)
     (make-lexicographic-<
      (λ (w) (length (get-field wrap-spec w)))
      (λ (w) (send w max-content start-tip end-tip direction))))))

(define (local-wraps-alt-< start-tip end-tip direction xc width depth)
  (let ([score
         (λ (wrap) (+map (λ (wt fn) (* wt (fn wrap)))
                         (list (* 8 (expt 2 (* 2 depth))) 1)
                         (list
                          (λ (w) (+ 1 (length (get-field wrap-spec w))))
                          (λ (w) (send w max-content start-tip end-tip direction)))))])
    (λ (w1 w2) (< (score w1) (score w2)))))

(define sequence%
  (class inline-diagram%
    (init-field subs)
    (unless (> (length subs) 0)
      (raise-arguments-error 'sequence "must sequence at least one diagram"))
    (init-field [marker (list-ref '("†" "‡" "◊" "¤" "*") (random 5))])

    (field [wraps
            (map
             (λ (wrap-spec)
               (new wrapped-sequence% [subs subs] [wrap-spec wrap-spec] [marker marker]))
             (combinations (range (- (length subs) 1))))])

    #;(field
     [global-wraps-measures
      (let ([measured
             (map
              (λ (x)
                (let* ([subs (map (λ (wm) (cdr (assq 'wrap wm))) (rest x))]
                       [sub-wrap-specs (map (λ (wm) (cdr (assq 'wrap-specs wm))) (rest x))]
                       [wrapped (new wrapped-sequence% [wrap-spec (first x)] [subs subs] [marker marker])])
                  (unless (~= (send wrapped max-content 'default 'default 'ltr)
                              (send wrapped min-content 'default 'default 'ltr))
                    (raise-arguments-error
                     'sequence%-global-wraps-measures
                     "max- and min-content must be equal for a globally wrapped diagram"
                     "max-content" (send wrapped max-content 'default 'default 'ltr)
                     "min-content" (send wrapped min-content 'default 'default 'ltr)))
                  (list
                   (cons 'natural-width (send wrapped min-content 'default 'default 'ltr))
                   (cons 'height (get-field height wrapped))
                   (cons 'wrap-specs (cons (first x) sub-wrap-specs))
                   (cons 'wrap wrapped))))
              (apply
               cartesian-product
               (combinations (range (- (length subs) 1)))
               (map (λ (s) (get-field global-wraps-measures s)) subs)))])
        (sort measured
              (global-wraps-< (argmin (λ (w) (cdr (assq 'natural-width w))) measured) (first wraps))))])

    (super-new [flex #t])

    (define (choose-wrap width start-tip end-tip direction depth)
      (let ([fitting
             (filter (λ (w) (<= (send w min-content start-tip end-tip direction) width)) wraps)])
         (if (empty? fitting)
             (argmin (λ (w) (send w min-content start-tip end-tip direction)) wraps)
             (first (sort fitting
                          ((local-wraps-<) start-tip end-tip direction
                                           (max-content start-tip end-tip direction)
                                           width depth))))))

    (define/augride (min-content start-tip end-tip direction)
      (apply min (map (λ (w) (send w min-content start-tip end-tip direction)) wraps)))

    (define/augride (max-content start-tip end-tip direction)
      (send (first wraps) max-content start-tip end-tip direction))

    #;(define/public (lay-out-global width start-tip end-tip direction)
      (send
       (let* ([natural-width (λ (w) (cdr (assq 'natural-width w)))]
              [fitting (filter (λ (w) (<= (natural-width w) width)) global-wraps-measures)])
         (cdr (assq 'wrap (if (empty? fitting) (argmin natural-width global-wraps-measures) (first fitting)))))
       lay-out width start-tip end-tip direction))

    (define/augride (lay-out width start-tip end-tip direction depth)
      (send (choose-wrap width start-tip end-tip direction depth)
            lay-out width start-tip end-tip direction depth))))

(define (+map f . ls) (apply + (apply map f ls)))

(define (distribute-linear max-contents remaining-proportions)
  (make-list (length max-contents) (λ (t) t)))

(define (distribute-quadratic max-contents remaining-proportions)
  (let* ([k 2]
         [remaining-contents (map * max-contents remaining-proportions)]
         [remaining-proportions-pow (map (λ (p) (expt p k)) remaining-proportions)]
         [basis (/ (apply + remaining-contents)
                   3 (apply + (map * remaining-contents remaining-proportions-pow)))])
    (map (λ (pp) (λ (t) (+ (expt t 3) (* 2 (- 1 t) (expt t 2)) (* 3 basis pp (expt (- 1 t) 2) t))))
         remaining-proportions-pow)))

(define (second-last l) (list-ref l (- (length l) 2)))
(define ((piecewise-linear-interpolate xs ys) x)
  (cond
    [(< x (first xs))
     (linear-interpolate (first xs) (first ys) (second xs) (second ys) x)]
    [(<= (last xs) x)
     (linear-interpolate (second-last xs) (second-last ys) (last xs) (last ys) x)]
    [else
     (let loop ([xis (map cons (drop-right xs 1) (rest xs))]
                [yis (map cons (drop-right ys 1) (rest ys))])
       (let ([xlo (caar xis)] [xhi (cdar xis)] [ylo (caar yis)] [yhi (cdar yis)])
         (if (and (<= xlo x) (< x xhi))
             (linear-interpolate xlo ylo xhi yhi x)
             (loop (cdr xis) (cdr yis)))))]))

(define (distribute-extreme max-contents remaining-proportions)
  (if (andmap zero? remaining-proportions)
      (make-list (length remaining-proportions) (λ (t) (linear-interpolate 0 0 1 1 t)))
      (let* ([ctrl-ys (map (λ (p)
                             (append
                              (map (λ (q) (max 0 (- 1 (/ q p))))
                                   (sort (filter-not zero? remaining-proportions) >))
                              '(1)))
                           remaining-proportions)]
             [ctrl-ys-transpose (apply map list ctrl-ys)]
             [ctrl-xs (map (λ (ys) (+map * ys max-contents remaining-proportions))
                           ctrl-ys-transpose)]
             [ctrl-xs (map (λ (x) (/ x (last ctrl-xs))) ctrl-xs)])
        (map
         (λ (ys) (piecewise-linear-interpolate ctrl-xs ys))
         ctrl-ys))))

(define distribute-fun (make-parameter distribute-linear))

(define wrapped-sequence%
    (class inline-diagram%
    (init-field subs wrap-spec marker)
    (unless (andmap (λ (break) (<= 0 break (- (length subs) 2))) wrap-spec)
      (raise-arguments-error
       'wrapped-sequence "wrap-spec breaks must be in [0, (- (length subs) 2)]"))
    (define wrapped-subs (break-at subs wrap-spec))

    (super-new [flex #t])

    (define (start-vertical? start-tip direction)
      (and (empty? wrap-spec)
           (vertical? start-tip)
           (memq (justify-content)
                 (list jc-start jc-space-between (if (eq? direction 'ltr) jc-left jc-right)))))

    (define (end-vertical? end-tip direction)
      (and (empty? wrap-spec)
           (vertical? end-tip)
           (memq (justify-content)
                 (list jc-end jc-space-between (if (eq? direction 'ltr) jc-right jc-left)))))

    (define (start-vertical-space? start-tip direction)
      (and (empty? wrap-spec) (vertical? start-tip) (not (start-vertical? start-tip direction))))
    (define (end-vertical-space? end-tip direction)
      (and (empty? wrap-spec) (vertical? end-tip) (not (end-vertical? end-tip direction))))

    (define (extra-width start-tip end-tip direction)
      (+ (if (start-vertical-space? start-tip direction) (min-strut-width) 0)
         (if (end-vertical-space? end-tip direction) (min-strut-width) 0)
         (if (empty? wrap-spec) 0
             (+ (* 2 (get-field physical-width (new text-marker% [label marker])))
                (+map (λ (tip r) (match tip
                                   [`(physical . ,p) #:when (not (= p r)) (* 3/2 (min-strut-width))]
                                   [else 0]))
                      (list start-tip end-tip) '(0 1))))))

    (define-values (sub-start-tips sub-end-tips)
      (values
       (λ (start-tip direction row-len)
         (let ([sub-tip (cdr (assq 'value (align-items)))])
           (append (list (if (start-vertical? start-tip direction) 'vertical sub-tip))
                   (make-list (- row-len 1) sub-tip))))
       (λ (end-tip direction row-len)
         (let ([sub-tip (cdr (assq 'value (align-items)))])
           (append (make-list (- row-len 1) sub-tip)
                   (list (if (end-vertical? end-tip direction) 'vertical sub-tip)))))))

    (define (-content which start-tip end-tip direction)
      (+ (apply max (map (λ (row)
                           (+ (+map (λ (s st et) (dynamic-send s which st et direction))
                                    row
                                    (sub-start-tips start-tip direction (length row))
                                    (sub-end-tips end-tip direction (length row)))
                              (* (- (length row) 1) (min-gap))))
                         wrapped-subs))
         (extra-width start-tip end-tip direction)))
    (define/augride (min-content start-tip end-tip direction)
      (-content 'min-content start-tip end-tip direction))
    (define/augride (max-content start-tip end-tip direction)
      (-content 'max-content start-tip end-tip direction))

    (field [height (get-field physical-height (lay-out (max-content 'default 'default 'ltr)))])

    (define/augride (lay-out width start-tip end-tip direction depth)
      (let*
          ([available-width (- (max width (min-content start-tip end-tip direction))
                               (extra-width start-tip end-tip direction))]
           [lay-out-row
            (λ (row)
              (let* (; available-width will be rebound at every step
                     [available-width (- available-width (* (min-gap) (- (length row) 1)))]
                     [start-tips (sub-start-tips start-tip direction (length row))]
                     [end-tips (sub-end-tips end-tip direction (length row))]
                     ; w- bindings will add up to the width for each sub
                     [w-min-contents (map (λ (s st et) (send s min-content st et direction))
                                          row start-tips end-tips)]
                     [available-width (- available-width (apply + w-min-contents))]
                     [max-contents (map (λ (s st et) (send s max-content st et direction))
                                        row start-tips end-tips)]
                     [remaining-contents (map - max-contents w-min-contents)]
                     [remaining-proportions (map (λ (rc mc) (if (= 0 mc) 0 (/ rc mc)))
                                                 remaining-contents max-contents)]
                     [total-remaining-contents (apply + remaining-contents)]
                     [available-remaining-proportion
                      (if (~= total-remaining-contents 0) 0
                          (/ (min available-width total-remaining-contents)
                             total-remaining-contents))]
                     [distribute-funs ((distribute-fun) max-contents remaining-proportions)]
                     [w-grow-contents (map (λ (f rc) (* rc (f available-remaining-proportion)))
                                           distribute-funs
                                           remaining-contents)]
                     #;[check (unless (~= (apply + w-grow-contents) (min available-width total-remaining-contents))
                                (raise-arguments-error 'distribute "error"
                                                       "min-contents" w-min-contents
                                                       "max-contents" max-contents
                                                       "grow" (apply + w-grow-contents)
                                                       "grow-contents" w-grow-contents
                                                       "total" total-remaining-contents
                                                       "available" available-width))]
                     [available-width (- available-width (apply + w-grow-contents))]
                     ; x- bindings will add up to the absorbed space
                     [x-initial (* (flex-absorb) available-width)]
                     [available-width (- available-width x-initial)]
                     [flex-max-contents
                      (map (λ (s xc) (if (get-field flex s) xc 0)) row max-contents)]
                     [total-flex-max-contents (apply + flex-max-contents)]
                     [w-grow-flex
                      (if (= total-flex-max-contents 0) (make-list (length row) 0)
                          (map (λ (fmc) (* available-width (/ fmc total-flex-max-contents)))
                               flex-max-contents))]
                     [x-post-flex (- available-width (apply + w-grow-flex))]
                     [w-total (map + w-min-contents w-grow-contents w-grow-flex)]
                     [sub-layouts (map (λ (s w st et) (send s lay-out w st et direction (+ 1 depth)))
                                       row w-total start-tips end-tips)])
                (justify-layouts-without-zero-hstruts
                 (+ x-initial x-post-flex)
                 (directional-reverse direction sub-layouts)
                 direction)))])
        (if (empty? wrap-spec)
            (new happend-layout%
                 [subs (directional-reverse
                        direction
                        (pre-post-pend (and (start-vertical-space? start-tip direction)
                                            (new hspace% [direction direction]))
                                       (lay-out-row (first wrapped-subs))
                                       (and (end-vertical-space? end-tip direction)
                                            (new hspace% [direction direction]))))]
                 [fuse? #t] [direction direction])
            (new
             vappend-inline-layout%
             [subs (map lay-out-row wrapped-subs)]
             [tip-specs (map cons '(left right)
                             (directional-reverse direction (list start-tip end-tip)))]
             [direction direction] [style 'marker]
             [marker (new text-marker% [direction direction] [label marker])]))))))

(define (desugar expr [splice? #t])
  ; to avoid passing splice? to each call
  (let desugar ([expr expr])
    (match expr
      [(? string?) (if (and (string-prefix? expr "[") (string-suffix? expr "]"))
                       (list 'nonterm (substring expr 1 (- (string-length expr) 1)))
                       (list 'term expr))]
      ['() '(epsilon)]
      ['epsilon '(epsilon)]
      ['ellipsis '(ellipsis)]
      [(or (list* '<> '+ exprs) (list* '+ exprs))
       (case (length exprs)
         [(0) '(epsilon)]
         [(1) (desugar (first exprs))]
         [(2) `(<> + ,(desugar (first exprs)) ,(desugar (second exprs)))]
         [else `(<> + ,(desugar (first exprs)) ,(desugar `(<> + ,@(rest exprs))))])]
      [(list* '<> '- exprs)
       (case (length exprs)
         [(2) `(<> - ,(desugar (first exprs)) ,(desugar (second exprs)))]
         [else (raise-arguments-error
                'desugar "negative polarity stack takes exactly two arguments"
                "exprs" exprs)])]
      [(list* 'seq exprs)
       (cons 'seq
             (if splice?
                 (append-map (λ (e) (match (desugar e)
                                      [(list* 'seq exprs) exprs]
                                      [exprs (list exprs)])) exprs)
                 (map desugar exprs)))]
      [(? list?) (desugar (cons 'seq expr))]
      [_ expr])))

(define (diagram expr [flex-stacks? #t] [splice? #t] [marker #f])
  (let rec ([expr (desugar expr splice?)] [in-stack? #f])
    (match expr
      ['(epsilon) (new epsilon%)]
      ['(ellipsis) (new ellipsis%)]
      [(list 'term label) (new station% [terminal? #t] [label label])]
      [(list 'nonterm label) (new station% [terminal? #f] [label label])]
      [(list '<> polarity expr-top expr-bot)
       (new stack% [diag-top (rec expr-top #t)] [diag-bot (rec expr-bot #t)]
            [polarity polarity] [flex (or flex-stacks? in-stack?)])]
      [(list* 'seq exprs)
       (if marker
           (new sequence% [subs (map (λ (e) (rec e #f)) exprs)] [marker marker])
           (new sequence% [subs (map (λ (e) (rec e #f)) exprs)]))])))

(define figure-margin (make-parameter 1))

(define ps-setup
  (let ([pss (new ps-setup%)])
    (send* pss
      (set-mode 'file)
      (set-scaling 0.78 0.78))
    pss))

(define (ensure-suffix str suffix)
  (if (string-suffix? str suffix) str (string-append str suffix)))

(define (print-layout-svg! layout filename)
  (let* ([width (+ (* 2 (figure-margin))
                   (get-field physical-width layout))]
         [height (max 5 (+ (* 2 (figure-margin)) (get-field physical-height layout)))]
         [my-svg-dc (new svg-dc%
                         [width width]
                         [height height]
                         [output (ensure-suffix filename ".svg")]
                         [exists 'truncate])])
    (send my-svg-dc start-doc "")
    (send my-svg-dc start-page)
    (send my-svg-dc set-smoothing 'smoothed)
    (for-each (λ (cmd) (apply dynamic-send my-svg-dc cmd))
              (send layout render (figure-margin)(figure-margin)))
    (send my-svg-dc end-page)
    (send my-svg-dc end-doc)))

(define (print-layout-pdf! layout filename [description #f])
  (parameterize ([current-ps-setup ps-setup])
    (let* ([description-height (if description 20 0)]
           [description-width (if description 80 0)]
           [width (+ (* 2 (figure-margin))
                     (max (get-field physical-width layout)
                          description-width))]
           [height (max 5 (+ (* 2 (figure-margin))
                             (get-field physical-height layout)
                             description-height))]
           [my-pdf-dc (new pdf-dc%
                           [interactive #f]
                           [width (* width 0.78)]
                           [height (* height 0.78)]
                           [output (ensure-suffix filename ".pdf")])])
      (send my-pdf-dc start-doc "")
      (send my-pdf-dc start-page)
      (send my-pdf-dc set-smoothing 'smoothed)
      (for-each (λ (cmd) (apply dynamic-send my-pdf-dc cmd))
                (send layout render (figure-margin) (+ (figure-margin) description-height)))
      (when description
        (send my-pdf-dc set-font (the-nonterminal-font))
        (send my-pdf-dc draw-text description (figure-margin) (figure-margin) #t))
      (send my-pdf-dc end-page)
      (send my-pdf-dc end-doc))))
