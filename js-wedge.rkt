#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)

;(require rosette/solver/smt/cvc4)
;(current-solver (cvc4 #:path "./cvc4"))
;(output-smt "./tmp/")

(require "js.rkt")
(require "js-wat.rkt")

(define (wedge target [known '()] [candidate #f])
  (begin
    (define M-user (make-mis*))
    (define p
      (time
        (cond
          [candidate candidate]
          [(equal? target mis-sortraw) (js-expr-XL* #:depth 2)]
          [else (js-expr* #:depth 2)]
          )))

    (define (check-M M-user)
      (begin
        (assume (not (ormap (lambda (m) (m M-user)) known)))
;       (unless candidate
;         (assume (<= (mis-cost M-user) 3)))
        (define r1 (time ((misinterpreter M-user) p)))
        (define r2 (time ((misinterpreter (make-M (list target))) p)))
        (assert (<=> (target M-user) (equal? r1 r2)))
        ))

    (if candidate
      (begin
        (define sol (time (verify (check-M M-user))))
        (if (sat? sol)
          (begin
            (define M-bad (evaluate M-user sol))
            (map (lambda (a b) (printf "~a ~a\n" a b))
                 (cdr (vector->list (struct->vector M-bad)))
                 mis-names)
            (displayln ((misinterpreter M-bad) p))
            (displayln "DONE"))
          (begin
            (displayln "Okay!"))))
      (begin
        (define sol (time (synthesize #:forall M-user #:guarantee (check-M M-user))))
        (if (sat? sol)
          (begin
            (printf "Diagnostic program: ~a\n" (unsafe!js->string (evaluate p sol)))
            (printf "True output: ~a\n"
              (unsafe!js->string
                ((misinterpreter M-standard) (evaluate p sol))))
            (printf "Distractor output: ~a\n"
              (unsafe!js->string
                ((misinterpreter (make-M (list target))) (evaluate p sol))))
            (void (explain-program (evaluate p sol))))
          (begin
            (displayln "Synthesis returned UNSAT.")))))))

#|
(wedge mis-arrstrnull (list mis-arrstrbrackets))
(wedge mis-arrstrnull (list mis-arrstrbrackets)
  (js-object #t
    (list
      (op-+ (js-string '()) (js-object #t (list)))
      (op-+ (js-string '()) (js-object #t (list (js-null)))))))
|#

;(wedge mis-sortraw (list mis-<castsnum) (op-sort (js-object #t (list (js-number 20) (js-number 5)))))
;(wedge mis-sortraw (list mis-<castsnum))
;(wedge mis-+semistrict)
;(wedge mis-<castsnum '() (op-< (js-string '(1 0)) (js-string '(2))))
;(wedge mis-nanstrempty (list mis-+castnum) (op-+ (js-string '()) (js-number 'NaN)))
;(wedge mis-==boolcoerce (list mis-==strict) (op-== (js-boolean #t) (js-number 2)))
;(wedge mis-oneindexed '() (op-index (js-string '(1 0)) (js-number 1)))
;(wedge mis-asciicomma '() (op->= (js-string '(1)) (js-string '(COMMA))))
;(wedge mis-emptyarraytruthy '() (op-|| (js-object #t '()) (js-boolean #t)))

(provide wedge)