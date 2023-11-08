#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require "js.rkt")
(require "js-wat.rkt")

(begin
  (displayln "Build...")
  (define M-user (make-mis*))
  (define p (time (js-expr* #:depth 2)))

  (define (check-M M-user)
    (begin
      (displayln "Mis 1...")
      (define r1 (time ((misinterpreter M-user) p)))

      (displayln "Mis 2...")
      (define r2 (time ((misinterpreter M-standard) p)))

      ; (assume (<= (mis-cost M-user) 2))
      (assert (<=> (or (mis-oneindexed M-user) (mis-emptyarraytruthy M-user)) (not (equal? r1 r2))))
      ))

  (define sol
    (time (synthesize #:forall M-user
                      #:guarantee (check-M M-user))))

  (if (sat? sol)
    (begin
      (printf "Diagnostic program: ~a\n" (unsafe!js->string (evaluate p sol)))
      (printf "True output: ~a\n" (unsafe!js->string ((misinterpreter M-standard) (evaluate p sol))))
      (explain-program (evaluate p sol))
    )
    (begin
      (displayln "Synthesis returned UNSAT."))))

;(explain-program (op-== (op-+un (js-object #f '())) (js-number 'NaN)))
;(explain-program (op-typeof (op-?: (js-object #t '()) (js-null) (js-undefined))))
;(explain-program (op-index (js-string '(a b c)) (js-number 1)))
;(explain-program (op-index (op-sort (js-object #t (list (js-number 11) (js-number 9)))) (js-number 0)))
;(explain-program (op-index (op-sort (js-object #t (list (js-number 11) (js-number 10)))) (js-number 1)))
;(explain-program (op-?? (op-== (js-number 'NaN) (js-number 'NaN)) (js-number 3)))
;(explain-program (op-?? (js-number 'NaN) (js-number 3)))
;(explain-program (op-+ (js-object #f '()) (js-object #f '())))
;(explain-program (op-+ (js-object #t '()) (js-object #f '())))
;(explain-program (op-+ (js-object #f '()) (js-object #t '())))
;(explain-program (op-+ (js-object #t '()) (js-object #t '())))