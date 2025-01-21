#lang racket
(require racket/draw "angle-brackets.rkt")

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



(define my-svg-dc
  (new svg-dc% [width 500] [height 200] [output "trial.svg"] [exists 'truncate]))
(send my-svg-dc start-doc "")
(send my-svg-dc start-page)

(define my-target (make-bitmap 1100 500))
(define my-bitmap-dc
  (new bitmap-dc% [bitmap my-target]))
;; (send my-bitmap-dc scale 2 2)

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
(define diag2 (new sequence%
                   [subs (list (new station% [terminal? #f] [label "good"])
                               (new station% [terminal? #f] [label "bye"])
                               (new station% [terminal? #t] [label "!!!"]))]))
(define diag2-5 (new station% [terminal? #f] [label "foreschmack"]))
(define diag3 (new stack% [diag-top diag1] [diag-bot diag2] [polarity '-]))

(define diag4 (new station% [terminal? #t] [label "Vermont"]))
(define diag5 (new station% [terminal? #f] [label "parametricity"]))
(define diag6 (new stack% [diag-top diag4] [diag-bot diag5] [polarity '+]))

(define diag7 (new stack% [diag-top diag3] [diag-bot diag6] [polarity '+] [flex #f]))
(define layout7 (parameterize ([justify-content flex-end])
                  (send diag7 lay-out 150 '(logical . 1.6) '(logical . 0) 'rtl)))

(define diag8 (new sequence% [subs (list diag1 diag2-5 diag7 diag4 diag5)] [min-gap 0]))
(define layout8 (parameterize ([justify-content space-evenly])
                  (send diag8 lay-out 450 'default 'default 'ltr)))


(println "width")
(println (get-field physical-width layout8))
(for-each (lambda (cmd) (apply dynamic-send my-bitmap-dc cmd)) (send layout8 render 20 20))
my-target

(send my-svg-dc end-page)
(send my-svg-dc end-doc)
