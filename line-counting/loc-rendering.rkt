 the-atom-text-pen the-atom-box-pen the-strut-pen the-atom-box-brush
arrow-threshold
(define the-atom-text-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-atom-box-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-strut-pen
  (make-parameter (make-pen #:color "black" #:width 1 #:style 'solid)))
(define the-atom-box-brush
  (make-parameter (make-brush #:style 'transparent)))
(define arrow-threshold (make-parameter 5))
(abstract render #;(render x y))))
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
                inner-up-arc outer-up-arc outer-down-arc inner-down-arc))))))
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
         (list (inner-down-arc top-tip-y))
         (append-map (λ (ty) (list (inner-up-arc ty))) sub-tip-ys)
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
         (list (vline top-tip-y bot-tip-y)))))
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
          '()))
  (define/override (render x y) '())))
(define/override (render x y)
      (append
       `((set-pen ,(the-strut-pen))
         (draw-line ,x ,(+ y y-diff) ,(+ x physical-width) ,(+ y y-diff)))
       (if (or (and always-arrow (>= physical-width (* 6 base-diff)))
               (>= physical-width (* (min-strut-width) (arrow-threshold))))
           (let ([x-diff ((if (eq? direction 'ltr) + -) (* base-diff 3))])
             `((draw-lines
                ((,(- x-diff) . ,(- y-diff)) (,x-diff . 0) (,(- x-diff) . ,y-diff))
                ,(+ x (/ physical-width 2) (* x-diff 0.3)) ,(+ y y-diff))))
           '())))
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
          (set-font ,(the-font))
          (draw-text ,label ,text-x ,text-y #t)
          (set-pen ,(the-atom-box-pen))
          (set-brush ,(the-atom-box-brush))
          (,(if terminal? 'draw-rounded-rectangle 'draw-rectangle)
           ,box-x ,box-y ,box-width ,box-height))))
    (define/override (render x y)
      (let* ([box-x x]
             [box-y y]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y y-alignment-magic)])
        `((set-pen ,(the-atom-text-pen))
          (set-font ,(the-font))
          (draw-text ,label ,text-x ,text-y #t))))
    (define/override (render x y)
      (append-map (lambda (sub sx sy) (send sub render (+ x sx) (+ y sy)))
                  subs sub-xs sub-ys))
