#lang racket

(require "js.rkt")
(require "js-wedge.rkt")


(define n (string->number (vector-ref (current-command-line-arguments) 0)))
(define m
  (list-ref mis-names n))

(define f
  (format "out/~a.txt" (~r n #:min-width 2 #:pad-string "0")))

(with-output-to-file f #:exists 'replace
  (lambda ()
    (begin
      (displayln m)
      (wedge m))))