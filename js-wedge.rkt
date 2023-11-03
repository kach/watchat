#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require "js.rkt")

(define (explain-program p)
    (printf "Surprising behavior observed by user:\n  ~a → ~a\n"
            (unsafe!js->string p)
            (unsafe!js->string ((misinterpreter M-standard) p)))

    (define M-user (make-mis*))

    (assert (not (equal? ((misinterpreter M-user) p)
                         ((misinterpreter M-standard) p))))

    (define cost (mis-cost M-user))

    (define (loop! [n 5])
      (newline)
      (define sol (optimize #:minimize (list cost) #:guarantee #t))
      (unless (unsat? sol)
        (assert (not (equal? M-user (evaluate M-user sol))))
        (assert (<= cost (evaluate cost sol)))
        (printf "Found explanation with cost $~a:\n" (evaluate cost sol))
        (define lyst (cdr (vector->list (struct->vector (evaluate M-user sol)))))
        (map displayln (map cdr (filter car (map cons lyst mis-names))))
        (printf "Student would expect: ~a\n"
                (unsafe!js->string ((misinterpreter (evaluate M-user sol)) p)))
        (unless (= n 1) (loop! (- n 1)))
      ))
    (loop!))

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
;(explain-program (op--un (js-object #t (list (js-boolean #f) (js-boolean #f)))))