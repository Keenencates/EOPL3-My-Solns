#lang racket

(define (nth-element lst n)
  (nth-element-helper lst lst n n))

(define (nth-element-helper lst olst n on)
  (cond
    [(null? lst)(report-list-too-short olst on)]
    [(zero? n)(first lst)]
    [else (nth-element-helper (rest lst) olst (- n 1) on)]))

(define (report-list-too-short lst n)
  (error 'nth-element "~s does not have ~s elements." lst (+ n 1)))

(nth-element '(a b c d e) 9)