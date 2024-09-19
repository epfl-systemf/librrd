#lang racket
(require racket/draw)
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

;; TODO: should be reversed in the expr before (diagram), right?
(define (diagram expr)
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
              [diags (map diagram exprs)]
              [align-items 'dum]
              [justify-content space-evenly]))]
    [(list '<> polarity expr1 expr2)
     (let* ([diag2 (diagram expr2)]
            [maybe-rev-diag2 (if (eq? polarity '-) (reverse-diagram diag2) diag2)])
       (new vappend-block-diagram%
            [diag-top (diagram expr1)]
            [diag-bot maybe-rev-diag2]
            [align-items center]))]))

(define MIN-STRUT-WIDTH 6)

(define diagram%
  (class object% (super-new)
    (init-field natural-width
                natural-height
                num-logical-rows ; (('left . X) ('right . X))
                direction)
    ; to SVG? HTML? TiKZ? how to have different backends?
    #;(define/public (lay-out left-tip right-tip width))
    (abstract lay-out)
    ;; A diagram can provide a tip at height:
    ;;   * 'top             an upwards tip
    ;;   * ([0, lh] 'L)     integers are exactly at logical rows, and
    ;;                      other numbers are logically interpolated such that
    ;;                      0.5 is halfway between the subdiagrams on rows 0 and 1
    ;;                      (always a horizontal tip)
    ;;   * ([0, ph] 'P)     as above but physical
    ;;   * 'bot             a downwards tip
    ;;   * 'default         the default among the above
    ;;
    ;; These are provided at both sides (left and right) of the diagram.
    (abstract tip? #;(tip? side spec))))

(define layout%
  (class object% (super-new)
    (init-field width height draw-proc)
    (define/public-final (draw! dc x y)
      (with-origin-delta dc x y
        (draw-proc dc)))))

(define block-diagram%
  (class diagram% (super-new)))

(define (set-random-color! dc)
  (send dc set-pen (make-color (random 256) (random 256) (random 256)) 1 'solid))

;; a type of align-items
(define (center total-width this-width)
  (/ (- total-width this-width) 2))

(define vappend-block-diagram%
  (class block-diagram%
    (init-field diag-top
                diag-bot
                align-items
                ;; always packed tightly along vertical axis
                #;justify-content
                [gap 8])
    
    (super-new [direction (get-field direction diag-top)]
               [natural-width (+ (max (get-field natural-width diag-top)
                                      (get-field natural-width diag-bot))
                                 (* 2 MIN-STRUT-WIDTH))]
               [natural-height (+ (get-field natural-height diag-top)
                                  (get-field natural-height diag-bot)
                                  gap)]
               [num-logical-rows
                (let ([peek (lambda (diag side tip)
                              (if (send diag tip? side tip)
                                  (cdr (assoc side (get-field num-logical-rows diag)))
                                  1))])
                  (map
                   (lambda (side)
                     (cons side (+ (peek diag-top side 'bot) (peek diag-bot side 'top))))
                   '(left right)))])
    (inherit-field natural-height)

    #;(define (self-tip-to-child child spec)
      (case spec
        [(default 0) spec]
        [(top bot) 'default]
        [else 'default]))

    (define/override (tip? side spec)
      (case spec
        [(default 0) (send diag-top tip? side spec)]
        [(top) (send diag-top tip? side 'default)]
        [(bot) (+ (get-field natural-height diag-top)
                  gap
                  (send diag-bot tip? side 'default))]
        [else #f]))

    (define/override (lay-out left-tip right-tip width)
      ; let W := max(diag1.w, diag2.w)
      ; let aligned-x := w => x + (W - w)/2
      ;   (or other strategy per align-items)
      ;
      ; use vtips if possible, else htips (choose per default) + a little padding
      ; draw diag1 at (aligned-x diag1.w), y, chosen tips
      ; draw diag2 at (aligned-x diag2.w), y + diag1.h + gap, chosen tips
      ; draw self sides (+ padding if needed) per specified tips
      (let* ([dc (new record-dc%)]
             [left-tip-width (if (memq left-tip '(top bot)) 0 MIN-STRUT-WIDTH)]
             [right-tip-width (if (memq right-tip '(top bot)) 0 MIN-STRUT-WIDTH)]
             ;; TODO: non-default tips per align-items
             [sub-width (- width left-tip-width right-tip-width)]
             [try-layout-top (send diag-top lay-out 'bot 'bot sub-width)]
             [try-layout-bot (send diag-bot lay-out 'top 'top sub-width)]
             [try-effective-width-top (get-field width try-layout-top)]
             [try-effective-width-bot (get-field width try-layout-bot)]
             [sub-effective-width (max try-effective-width-top try-effective-width-bot
                                       (- width left-tip-width right-tip-width))]
             [layout-top (send diag-top lay-out 'bot 'bot sub-effective-width)]
             [layout-bot (send diag-bot lay-out 'top 'top sub-effective-width)]
             [effective-width-top (get-field width layout-top)]
             [effective-width-bot (get-field width layout-bot)]
             [sub-x-left left-tip-width]
             [sub-x-right (+ sub-x-left sub-effective-width)]
             [x-top (+ sub-x-left (align-items sub-effective-width effective-width-top))]
             [x-bot (+ sub-x-left (align-items sub-effective-width effective-width-bot))]
             [y-tip-top-left (send diag-top tip? 'left 'bot)]
             [y-tip-top-right (send diag-top tip? 'right 'bot)]
             [effective-height-top (get-field height layout-top)]
             [effective-height-bot (get-field height layout-bot)]
             [y-top 0]
             [y-bot (+ y-top effective-height-top gap)]
             #;[effective-height (+ effective-height-top gap effective-height-bot)]
             [effective-height (+ y-bot effective-height-bot (- y-top))]
             [y-tip-bot-left (+ y-bot (send diag-bot tip? 'left 'top))]
             [y-tip-bot-right (+ y-bot (send diag-bot tip? 'right 'top))]
             [effective-width (+ sub-effective-width left-tip-width right-tip-width)])

        (set-random-color! dc)

        ; top with horizontals
        (send dc draw-line
              sub-x-left y-tip-top-left
              x-top y-tip-top-left)
        (send layout-top draw! dc x-top y-top)
        (send dc draw-line
              (+ x-top effective-width-top) y-tip-top-right
              sub-x-right y-tip-top-right)

        ; bot with horizontals
        (send dc draw-line
              sub-x-left y-tip-bot-left
              x-bot y-tip-bot-left)
        (send layout-bot draw! dc x-bot y-bot)
        (send dc draw-line
              (+ x-bot effective-width-bot) y-tip-bot-right
              sub-x-right y-tip-bot-right)
        
        ; left vertical
        (send dc draw-line
              sub-x-left y-tip-top-left
              sub-x-left y-tip-bot-left)
        ; right vertical
        (send dc draw-line
              sub-x-right y-tip-top-right
              sub-x-right y-tip-bot-right)
        ; left tip
        (unless (memq left-tip '(top bot))
          (send dc draw-line
                0 (tip? 'left left-tip)
                sub-x-left (tip? 'left left-tip)))
        ; right tip
        (unless (memq right-tip '(top bot))
          (send dc draw-line
                sub-x-right (tip? 'right right-tip)
                effective-width (tip? 'right right-tip)))

        (new layout% [width effective-width] [height effective-height]
             [draw-proc (send dc get-recorded-procedure)])))))

(define text-measurement-dc (new bitmap-dc% [bitmap #f]))
(send text-measurement-dc set-font
      (send the-font-list find-or-create-font
            12 "Overpass Mono" 'modern 'normal 'normal))

(define (text-width text)
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    width))

(define (text-height text)
  (let-values ([(width height descend extra)
                (send text-measurement-dc get-text-extent text #f #t)])
    height))

(define atomic-block-diagram%
  (class block-diagram%
    (init-field terminal? label [padding-x 6] [padding-y 4])
    (field [label-width (text-width label)] [label-height (text-height label)])
    (super-new
     [natural-width (+ label-width (* 2 padding-x) (* 2 MIN-STRUT-WIDTH))]
     [natural-height (+ label-height (* 2 padding-y))]
     [num-logical-rows '((left . 1) (right . 1))])
    (inherit-field natural-width natural-height)

    (define/override (tip? side spec)
      (case spec
        [(default 0 top bot) (/ natural-height 2)]
        [else #f]))

    (define/override (lay-out left-tip right-tip width)
      ;; requested tips and width don't matter
      (let* ([dc (new record-dc%)]
             ;; these are all relative coördinates!
             [box-width (+ label-width (* 2 padding-x))]
             [box-height natural-height]
             [box-x MIN-STRUT-WIDTH]
             [box-y 0]
             [text-x (+ box-x padding-x)]
             [text-y (+ box-y padding-y)]
             [struts-y (/ natural-height 2)]
             [lstrut-lx 0]
             [lstrut-rx (+ lstrut-lx MIN-STRUT-WIDTH)]
             [rstrut-lx (+ MIN-STRUT-WIDTH box-width)]
             [rstrut-rx (+ rstrut-lx MIN-STRUT-WIDTH)])
        (set-random-color! dc)
        (send dc set-pen "black" 1 'solid)
        (send dc set-brush "white" 'transparent)
        (send dc set-font
              (send text-measurement-dc get-font))
        (if terminal?
            (send dc draw-rounded-rectangle
                  box-x box-y box-width box-height)
            (send dc draw-rectangle
                  box-x box-y box-width box-height))
        (send dc draw-text label text-x text-y #t)
        (send dc draw-line lstrut-lx struts-y lstrut-rx struts-y)
        (send dc draw-line rstrut-lx struts-y rstrut-rx struts-y)
        ;; does not close over any fields except natural dims
        (new layout% [width natural-width] [height natural-height]
             [draw-proc (send dc get-recorded-procedure)])))))

(define empty-block-diagram%
  (class block-diagram%
    (super-new [natural-width 0] [natural-height 1] [num-logical-rows '((left . 1) (right . 1))])
    (inherit-field natural-height)
    (define/override (tip? side spec)
      (case spec
        [(default 0 top bot) 0]
        [else #f]))
    (define/override (lay-out left-tip right-tip width)
      (let ([dc (new record-dc%)])
        (send dc set-pen "black" 1 'solid)
        (set-random-color! dc)
        (send dc draw-line 0 0 width 0)
        (new layout% [width width] [height natural-height]
             [draw-proc (send dc get-recorded-procedure)])))))

;; An inline diagram can only provide one horizontal tip at each end
;; of the diagram (left and right), aligned with the single row.
(define inline-diagram%
  (class diagram% (super-new)))

;; a type of justify-content
; contract: must not exceed total-space
; contract: total-space must be at least (* min-gap num-subs)
(define (space-evenly total-space num-subs min-gap)
  (let* ([each-basis (/ total-space (+ num-subs 1))]
         [each-mid-space (max min-gap each-basis)]
         [each-end-space (/ (- total-space (* each-mid-space (- num-subs 1))) 2)])
    (append (list each-end-space)
            (build-list (- num-subs 1) (lambda _ each-mid-space))
            (list each-end-space))))

(define happend-inline-diagram%
  (class inline-diagram%
    (init-field diags align-items justify-content [min-gap 0] [extra-width-absorb 0.2])
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
        [(default 0 top bot) (apply max (map (lambda (d) (send d tip? side 'default)) diags))]
        [else #f]))

    (define/override (lay-out left-tip right-tip width)
      (let* ([dc (new record-dc%)]
             [effective-width (max natural-width width)]
             [extra-width (- effective-width natural-width)]
             [sub-extra-width (* extra-width (- 1 extra-width-absorb))]
             [sub-total-natural-width
              (apply + (map (lambda (d) (get-field natural-width d)) diags))]
             [sub-layouts (map (lambda (d)
                                 (send d lay-out
                                       ;; TODO: get different tips per align-items
                                       'default
                                       'default
                                       (* sub-extra-width (/ (get-field natural-width d)
                                                             sub-total-natural-width))))
                               diags)]
             [sub-effective-widths (map (lambda (l) (get-field width l)) sub-layouts)]
             [sub-total-effective-width (apply + sub-effective-widths)]
             [self-extra-width (- effective-width sub-total-effective-width)]
             [self-extra-width-splits
              (justify-content self-extra-width (length diags) min-gap)]
             [struts-y (tip? 'dummy 'default)])
        (send dc set-pen "black" 1 'solid)
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
         sub-layouts
         (cdr self-extra-width-splits))
        (new layout% [width effective-width] [height natural-height]
             [draw-proc (send dc get-recorded-procedure)])))))

(define (reverse-diagram diag)
  (cond
    [(is-a? diag happend-inline-diagram%)
     (with-destruct-object-as diag
       (diags align-items justify-content min-gap extra-width-absorb)
       (new happend-inline-diagram%
            [diags (reverse (map reverse-diagram diag-diags))]
            [align-items diag-align-items]
            [justify-content diag-justify-content]
            [min-gap diag-min-gap]
            [extra-width-absorb diag-extra-width-absorb]))]
    [(is-a? diag vappend-block-diagram%)
     (with-destruct-object-as diag
       (diag-top diag-bot align-items gap)
       (new vappend-block-diagram%
            [diag-top (reverse-diagram diag-diag-top)]
            [diag-bot (reverse-diagram diag-diag-bot)]
            [align-items diag-align-items]
            [gap diag-gap]))]
    [else diag]))

(define my-svg-dc
  (new svg-dc% [width 500] [height 100] [output "trial.svg"] [exists 'truncate]))
(send my-svg-dc start-doc "")
(send my-svg-dc start-page)

(define my-target (make-bitmap 1000 500))
(define my-bitmap-dc
  (new bitmap-dc% [bitmap my-target]))
(send my-bitmap-dc scale 2 2)

(define expr `(<> - (seq (term "e") (term "a"))
                  (seq (term "ffffffffff") (nonterm "BCD") (term "g"))))
(define diag (diagram expr))
(define layout (send diag lay-out 'default 'default 300))

(define expr-seqseqseq '(seq (seq (seq (term "a")))))
(define diag-seqseqseq (diagram expr-seqseqseq))
(define layout-seqseqseq (send diag-seqseqseq lay-out 'default 'default 400))

(define expr-short '(<> - (<> + (term "c1") (term "c2")) (<> - (seq (<> + (term "a") (term "b")) (term "d")) (term "e"))))
(define layout-short (send (diagram expr-short) lay-out 'default 'default 80))

(define my-layout layout-short)
(displayln (get-field width my-layout))
(send my-layout draw! my-bitmap-dc 10 10)
my-target

(send my-svg-dc end-page)
(send my-svg-dc end-doc)
