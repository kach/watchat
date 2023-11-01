#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/destruct)
(require rosette/lib/angelic)
(require "js.rkt")

(current-bitwidth 5)

(begin
  (displayln "Build...")
  (define M-user (make-mis*))
  (define p (time (js-expr* #:depth 2)))

  (displayln "Mis 1...")
  (define r1 (time ((misinterpreter M-user) p)))

  (displayln "Mis 2...")
  (define r2 (time ((misinterpreter M-standard) p)))

  (displayln "Solving...")
  (define sol
    (time (synthesize #:forall M-user
                      #:guarantee
                      (begin
                       ;(assume (< (mis-cost M-user) 2))
                        (assume (mis-naneq M-user))
                        (assert (not (equal? r1 r2)))
                        ))))
  (printf "Diagnostic program: ~a\n" (unsafe!js->string (evaluate p sol)))
  (printf "True output: ~a\n" (unsafe!js->string (evaluate r2 sol)))
)




#;(begin
;(define p (op-== (op-+un (js-object #f '())) (js-number 'NaN)))
;(define p (op-typeof (op-?: (js-object #t '()) (js-null) (js-undefined))))
(define p (op-index (js-string '(a b c)) (js-number 1)))

(printf "Surprising behavior observed by user:\n  ~a → ~a\n"
        (unsafe!js->string p)
        (unsafe!js->string ((misinterpreter M-standard) p)))

(define M-user (make-mis*))

(display "Symeval... ")
(time
(assert (not (equal? ((misinterpreter M-user) p)
                     ((misinterpreter M-standard) p))))
)

(define cost (mis-cost M-user))

(define (loop! [n 5])
  (newline)
  (display "Running solver... ")
  (define sol (time (optimize #:minimize (list cost) #:guarantee #t)))
  (unless (unsat? sol)
    (assert (not (equal? M-user (evaluate M-user sol))))
    (assert (<= cost (evaluate cost sol)))
    (printf "Found explanation with cost $~a:\n" (evaluate cost sol))
    (define lyst (cdr (vector->list (struct->vector (evaluate M-user sol)))))
    (map displayln (map cdr (filter car (map cons lyst mis-names))))
    (unless (= n 1) (loop! (- n 1)))
  ))

(loop!)
)