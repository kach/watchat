#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require "js.rkt")

(define (mis->= M1 M2)
  (andmap
    (lambda (m) (=> (m M2) (m M1)))
    mis-names))

(define (explain-program p)
    (define true-out (unsafe!js->string ((misinterpreter M-standard) p)))
    (printf "-------\nSurprising behavior observed by user:\n  ~a → ~a\n"
            (unsafe!js->string p)
            true-out)

    (define M-user* (make-mis*))
    (define cost* (mis-cost M-user*))

    (define (loop! [acc '()] [n 5])
      (define sol (optimize #:minimize (list cost*)
                            #:guarantee
                            (assert (not (equal? ((misinterpreter M-user*) p)
                                                 ((misinterpreter M-standard) p))))))
      (if (unsat? sol)
        (begin
          (displayln "That's all!")
          acc)
        (begin
          (define M-user (evaluate M-user* sol))
          (define cost (evaluate cost* sol))

;         (assert (not (equal? M-user* M-user)))
          (assert
            (not
              (and (equal? ((misinterpreter M-user*) p)
                           ((misinterpreter M-user) p))
                   (mis->= M-user* M-user))))
          (assert (<= cost* cost))

          (define expected-out (unsafe!js->string ((misinterpreter M-user) p)))
          (define lyst (cdr (vector->list (struct->vector M-user))))
          (define explanation (map cdr (filter car (map cons lyst mis-texts))))

          (printf "Found explanation with cost* $~a:\n" cost)
          (printf "If student expects: ~a\n" expected-out)
          (printf "Then explain:\n")
          (map displayln explanation)
          (newline)

          (if (= n 1)
            (begin
              (displayln "(Possibly more...)")
              acc)
            (loop!
              (cons (make-hasheq (list (cons 'cost cost)
                                       (cons 'out expected-out)
                                       (cons 'text explanation))) acc)
              (- n 1))))
        ))

    (make-hasheq (list (cons 'out true-out) (cons 'explanations (loop!)))))

(provide explain-program)