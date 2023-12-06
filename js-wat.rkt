#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/angelic)
(require racket/trace)
(require "js.rkt")

(define (mis->= M1 M2)
  (andmap
    (lambda (m) (=> (m M2) (m M1)))
    mis-names))

(define (mis-apply-patch M patch)
  (apply mis
    (map (lambda (m) (and (m M) (not (m patch)))) mis-names)))


(define (explanation->string p M-user)
  (define log (mcons 'NEXT '()))
  (define cursor log)
  (define prev '())
  (define depth-now 0)

  (define (post! x)
    (set-mcar! cursor x)
    (set-mcdr! cursor (mcons 'NEXT '()))
    (set! cursor (mcdr cursor)))

  (define (push!)
    (set! depth-now (+ 1 depth-now))
    (set-mcar! cursor (mcons 'NEXT '()))
    (set-mcdr! cursor (mcons 'NEXT '()))
    (set! prev (cons (mcdr cursor) prev))
    (set! cursor (mcar cursor)))

  (define (pop!)
    (set! depth-now (- depth-now 1))
    (set! cursor (car prev))
    (set! prev (cdr prev)))

  (parameterize
    ([current-trace-notify void]
     [current-trace-print-args
      (lambda (name a kws kwargs depth)
        (printf "> ~a ~a\n" depth a)
        (if (member name mis-tags)
            (post! name)
            (begin
              (when (>= depth depth-now) (push!))
              (post! (unsafe!js->string (car a)))))
        (printf " "))]
     [current-trace-print-results
       (lambda (name results depth)
         (printf "< ~a ~a\n" depth results)
         (if (member name mis-tags) (void)
             (begin
               (post! (unsafe!js->string (car results)))
               (when (<= depth depth-now) (pop!))))
         (printf " "))])
    ((misinterpreter M-standard) p))

  (define (mlist->list x)
    (cond
      [(mpair? x) (cons (mlist->list (mcar x)) (mlist->list (mcdr x)))]
      [else x]))

  (define log-list (car (mlist->list log)))


  (define output '())
  (define reported-mis '())

  (define (traverse log)
    (define taint (ormap traverse (filter list? log)))

    (define tags
      (remove-duplicates
        (filter (lambda (x) (and (member x M-user) (not (member x reported-mis))))
          (drop-right (filter symbol? log) 1))))
    (set! reported-mis (append tags reported-mis))
    (define in (car log))
    (define out (car (take-right log 2)))
    (when (or taint (not (null? tags)))
      (set! output (cons (string-join (map tag->text tags) " ") output))
      (set! output (cons (format "Hence, ~a gives ~a." in out) output)))
    (or taint (not (null? tags))))
  (traverse log-list)
  (reverse output)
)


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

#|
          (define M-patch* (make-mis*))
          (define sol-patch (optimize
                              #:minimize (list (mis-cost M-patch*))
                              #:guarantee
                              (begin
                                (assert (equal? ((misinterpreter M-standard) p)
                                                ((misinterpreter (mis-apply-patch M-user M-patch*)) p)))
                                )))
          (define M-patch (evaluate M-patch* sol-patch))
|#

          (assert
            (not
              (and (equal? ((misinterpreter M-user*) p)
                           ((misinterpreter M-user) p))
                   (mis->= M-user* M-user))))

          (define expected-out (unsafe!js->string ((misinterpreter M-user) p)))
          (define lyst (cdr (vector->list (struct->vector M-user))))
          (define explanation-tags (map second (filter car (map list lyst mis-tags mis-texts))))
          (define explanation (explanation->string p explanation-tags))

          (printf "Found explanation with cost* $~a:\n" cost)
          (printf "Student expects: ~a\n" expected-out)
          (printf "Tags: ~a\n" explanation-tags)
          (newline)

          (if (= n 0)
            (begin
              (displayln "(Possibly more...)")
              acc)
            (loop!
              (cons (make-hasheq (list (cons 'cost cost)
                                       (cons 'out expected-out)
                                       (cons 'text explanation-tags))) acc)
              (- n 1))))
        ))

    (define es-raw (loop!))

    (define equivs (group-by (lambda (h) (hash-ref h 'out)) es-raw))
    (define es-done
      (map
        (lambda (es)
          (define expected-out (hash-ref (car es) 'out))
          (define cost (apply min (map (lambda (h) (hash-ref h 'cost)) es)))
          (define all-tags (append-map (lambda (h) (hash-ref h 'text)) es))
          (displayln all-tags)
          (define explanation
            (explanation->string p
              (remove-duplicates all-tags)))
          (make-hasheq (list (cons 'cost cost)
                             (cons 'out expected-out)
                             (cons 'text explanation))))
        equivs))

    (make-hasheq (list (cons 'out true-out) (cons 'explanations es-done)))
    )

(provide explain-program explanation->string)