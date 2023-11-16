#lang racket

(require "js.rkt")
(require "js-wedge.rkt")


(define n (string->number (vector-ref (current-command-line-arguments) 0)))
(define m
  (list-ref mis-names n))
(displayln m)
(define f
  (format "out/~a.txt" (~r n #:min-width 2 #:pad-string "0")))

(define (prereqs m)
  (cond
    [(equal? m mis-arrstrnull) (list mis-arrstrbrackets)]
    [(equal? m mis-arrstrundef) (list mis-arrstrbrackets)]
    [(equal? m mis-sortraw) (list mis-<castsnum)]
    [(equal? m mis-nanstrempty) (list mis-+castnum)]
    [(equal? m mis-==boolcoerce) (list mis-==strict)]
    [else '()]))

(with-output-to-file f #:exists 'replace
  (lambda ()
    (begin
      (displayln m)
      (wedge m (prereqs m)))))