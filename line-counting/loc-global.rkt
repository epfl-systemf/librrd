(field [height (get-field physical-height (lay-out (max-content 'default 'default 'ltr)))])
(field
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
(define/public (lay-out-global width start-tip end-tip direction)
      (send
       (let* ([natural-width (λ (w) (cdr (assq 'natural-width w)))]
              [fitting (filter (λ (w) (<= (natural-width w) width)) global-wraps-measures)])
         (cdr (assq 'wrap (if (empty? fitting) (argmin natural-width global-wraps-measures) (first fitting)))))
       lay-out width start-tip end-tip direction))
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
    (field [height (get-field physical-height init-ellipsis)])
    (field [global-wraps-measures
            (list (list (cons 'natural-width init-layout-width)
                        (cons 'height height)
                        (cons 'wrap-specs '(() . ()))
                        (cons 'wrap this)))])
    (field [height 0])
    (field [global-wraps-measures
            (list (list (cons 'natural-width 0)
                        (cons 'height height)
                        (cons 'wrap-specs '(() . ()))
                        (cons 'wrap this)))])
    (field [height (get-field physical-height (init-text-box))])
    (field [global-wraps-measures
            (list (list (cons 'natural-width (init-layout-width))
                        (cons 'height height)
                        (cons 'wrap-specs '(() . ()))
                        (cons 'wrap this)))])
    (field
     [height
      (if wrapped?
          (get-field physical-height (lay-out (max-content 'default 'default 'ltr)))
          'undefined)])
    (field
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
