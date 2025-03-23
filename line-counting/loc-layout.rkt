#lang racket
(require racket/draw racket/help)
(require (for-syntax racket/syntax))
(provide
 align-items ai-baseline ai-top ai-center ai-bottom
 justify-content jc-space-evenly jc-space-between jc-space-around
 jc-start jc-end jc-left jc-center jc-right
 min-strut-width row-gap min-gap flex-absorb
 the-font the-font-size text-width-correction
 layout% text-box% hstrut% happend-layout% ellipsis-marker%
 vappend-inline-layout% vappend-block-layout% vappend-forward-backward-layout%
 diagram% block-diagram% stack%
 inline-diagram% sequence% wrapped-sequence% station% epsilon%
 desugar diagram local-wraps-<)
(define (display-expr k) (begin (displayln k) k))
(define (~= x y) (> 0.0001 (abs (- x y))))
(define (pre-post-pend pre mid post)
  (append
   (if pre (if (list? pre) pre (list pre)) '())
   (if (list? mid) mid (list mid))
   (if post (if (list? post) post (list post)) '())))
(define ai-baseline '((name . baseline) (value . default)))
(define ai-top '((name . top) (value physical . 0)))
(define ai-center '((name . center) (value physical . 0.5)))
(define ai-bottom '((name . bottom) (value physical . 1)))
(define (directional-reverse direction l)
  (unless (memq direction '(ltr rtl)) (raise-argument-error 'direction "direction?" direction))
  ((if (eq? direction 'rtl) reverse identity) l))
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
(define align-items (make-parameter ai-top))
(define justify-content (make-parameter jc-space-evenly))
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
(define (min-strut-width) (* (+ (the-font-size) 12) 1/4))
(define (row-gap) (* (the-font-size) 2/3))
(define text-measurement-dc (new svg-dc% [width 1000] [height 1000] [output (open-output-nowhere)]))
(define text-width-correction (make-parameter identity))
(define (text-width text)
  (send text-measurement-dc set-font (the-font))
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    ((text-width-correction) width)))
(define (text-height text)
  (send text-measurement-dc set-font (the-font))
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    height))
(define min-gap (make-parameter 0))
(define flex-absorb (make-parameter 0.0))
(define layout%
  (class object% (super-new)
    (init-field physical-width physical-height
                num-rows
                tips
                [direction 'ltr])
    (define/pubment tip-y
      (case-lambda
        [(side) (cddr (assq side tips))]
        [(side spec) (inner #f tip-y side spec)]))))
(define (vertical? tip-spec) (eq? tip-spec 'vertical))
(define (direction-toggle d)
  (case d [(ltr) 'rtl] [(rtl) 'ltr] [else (raise-argument-error 'd "direction?" d)]))
(define (linear-interpolate xstart ystart xend yend x)
  (+ ystart (* (/ (- x xstart) (- xend xstart)) (- yend ystart))))
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
    ))
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
         "second sub" (second subs) "direction" direction)))))
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
          (super check-directions)))))
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
               [physical-width init-width] #;[direction pass-through])))
(define hstrut%
  (class inline-layout%
    (init-field [always-arrow #f])
    (define base-diff (* (the-font-size) 0.125))
    (define y-diff (* base-diff 2))
    (super-new [physical-height (* 2 y-diff)]
               [tips `((left default . ,y-diff) (right default . ,y-diff))]
               #;[physical-width pass-through] #;[direction pass-through])
    (inherit-field physical-width direction)
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
    (inherit-field physical-height)))
(define text-marker%
  (class text-box%
    (super-new [terminal? #f] [padding-x (min-strut-width)])
    (init-field [y-alignment-magic (* (the-font-size) 0.23)])
    (inherit-field label padding-x)))
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
       "subs" subs "direction" direction))))
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
    (define/pubment (lay-out width
                             [start-tip (cdr (assq 'value (align-items)))]
                             [end-tip (cdr (assq 'value (align-items)))]
                             [direction 'ltr])
      (inner (raise-user-error "lay-out not implemented!")
             lay-out width start-tip end-tip direction))))
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
      (if (eq? polarity '-)
          (for/sum ([tip (list start-tip end-tip)])
            (case tip
              [(vertical default (logical . 0) (physical . 0)) (min-strut-width)]
              [else (* 5/2 (min-strut-width))]))
          (for/sum ([tip (list start-tip end-tip)])
            (match tip
              [(or 'default (cons 'logical _) (cons 'physical _)) (* 3/2 (min-strut-width))]
              [else 0]))))
    (define (-content which start-tip end-tip direction)
      (+ (max (dynamic-send diag-top which 'vertical 'vertical direction)
              (dynamic-send diag-bot which 'vertical 'vertical direction))
         (maybe-tips-width start-tip end-tip)))
    (define/augride (min-content start-tip end-tip direction)
      (-content 'min-content start-tip end-tip direction))
    (define/augride (max-content start-tip end-tip direction)
      (-content 'max-content start-tip end-tip direction))
    (inherit-field flex)
    (define/augride (lay-out width start-tip end-tip direction)
      (let* ([clamped-width
              ((if flex (λ (a b) b) min)
               (max-content start-tip end-tip direction)
               (max (min-content start-tip end-tip direction) width))]
             [clamped-available-width (- clamped-width (maybe-tips-width start-tip end-tip))]
             [layout-top (send diag-top lay-out clamped-available-width 'vertical 'vertical direction)]
             [layout-bot
              (send diag-bot lay-out clamped-available-width 'vertical 'vertical
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
             [tip-specs (map cons '(left right)
                             (directional-reverse direction `(,start-tip ,end-tip)))])))))
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
    (define/augment (lay-out width start-tip end-tip direction)
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
    (define/augride (min-content _ #;start-tip __ #;end-tip) (init-layout-width))
    (define/augride (max-content _ #;start-tip __ #;end-tip) (init-layout-width))
    (define/augride (lay-out _ #;width start-tip end-tip direction)
      (let ([tip-strut (new hstrut% [physical-width (/ (min-strut-width) 2)] [direction direction])]
            [text-box (new text-box% [terminal? terminal?] [label label] [direction direction])])
        (new happend-layout% [subs (list tip-strut text-box tip-strut)] [direction direction])))))
(define epsilon%
  (class atomic-inline-diagram%
    (super-new [flex #t])
    (define/augride (lay-out width _ #;start-tip __ #;end-tip direction)
      (new hstrut% [physical-width width] [direction direction]))))
(define ellipsis%
  (class atomic-inline-diagram%
    (super-new [flex #f])
    (define init-ellipsis (new ellipsis-marker%))
    (define (init-layout-width)
      (+ (min-strut-width) (get-field physical-width init-ellipsis)))
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
(define (local-wraps-< start-tip end-tip direction)
  (make-lexicographic-<
   (λ (w) (length (get-field wrap-spec w)))
   (λ (w) (send w max-content start-tip end-tip direction))
   (λ (w) (send w min-content start-tip end-tip direction))))
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
    (super-new [flex #t])
    (define (choose-wrap width start-tip end-tip direction)
      (let ([fitting
             (filter (λ (w) (<= (send w min-content start-tip end-tip direction) width)) wraps)])
         (if (empty? fitting)
             (argmin (λ (w) (send w min-content start-tip end-tip direction)) wraps)
             (first (sort fitting (local-wraps-< start-tip end-tip direction))))))
    (define/augride (min-content start-tip end-tip direction)
      (apply min (map (λ (w) (send w min-content start-tip end-tip direction)) wraps)))
    (define/augride (max-content start-tip end-tip direction)
      (send (first wraps) max-content start-tip end-tip direction))
    (define/augride (lay-out width start-tip end-tip direction)
      (send (choose-wrap width start-tip end-tip direction)
            lay-out width start-tip end-tip direction))))
(define (+map f . ls) (apply + (apply map f ls)))
(define (distribute-linear max-contents remaining-proportions)
  (make-list (length max-contents) (λ (t) t)))
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
    (define/augride (lay-out width start-tip end-tip direction)
      (let*
          ([available-width (- (max width (min-content start-tip end-tip direction))
                               (extra-width start-tip end-tip direction))]
           [lay-out-row
            (λ (row)
              (let* (; available-width will be rebound at every step
                     [available-width (- available-width (* (min-gap) (- (length row) 1)))]
                     [start-tips (sub-start-tips start-tip direction (length row))]
                     [end-tips (sub-end-tips end-tip direction (length row))]
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
                     [available-width (- available-width (apply + w-grow-contents))]
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
                     [sub-layouts (map (λ (s w st et) (send s lay-out w st et direction))
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
  (let desugar ([expr expr])
    (match expr
      [(? string?) (if (and (string-prefix? expr "[") (string-suffix? expr "]"))
                       (list 'nonterm (substring expr 1 (- (string-length expr) 1)))
                       (list 'term expr))]
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
