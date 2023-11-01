#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/destruct)
(require rosette/lib/angelic)
(require "js.rkt")

(define reduce-expression (misinterpreter M-standard))

(define (unsafe!unit-test expr expected)
  (define out (reduce-expression expr))
  (define pass (equal? out expected))
  (printf "[~a] ~a → ~a\n"
    (if pass "pass" "FAIL")
    (unsafe!js->string expr)
    (unsafe!js->string expected))
  (unless pass
    (printf " | Got ~a\n" out)
    (printf " | Not ~a\n" expected)
    ))

(unsafe!unit-test
  (op-typeof (op-typeof (js-number 32)))
  (js-string '(s t r i n g)))

(unsafe!unit-test
  (op-=== (op-typeof (op-typeof (js-number 32))) (op-typeof (js-string '(a b c))))
  (js-boolean #t))

(unsafe!unit-test
  (op-=== (op-typeof (js-null)) (js-string '(n u l l)))
  (js-boolean #f))

(unsafe!unit-test
  (op-=== (js-number 'NaN) (js-number 'NaN))
  (js-boolean #f))

(unsafe!unit-test
  (op-! (js-number 'NaN))
  (js-boolean #t))

(unsafe!unit-test
  (op-?: (js-number 'NaN) (js-string '(t r u t h y)) (js-string '(f a l s e y)))
  (js-string '(f a l s e y)))

(unsafe!unit-test
  (op-?: (op-! (js-null)) (js-string '(t r u t h y)) (js-string '(f a l s e y)))
  (js-string '(t r u t h y)))

(unsafe!unit-test
  (op-== (js-null) (js-undefined))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (js-string '()) (js-number 0))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (js-string '(1 2 3)) (js-number 123))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (js-null) (js-number 0))
  (js-boolean #f))

(unsafe!unit-test
  (op-== (op-! (js-number 'NaN)) (js-string '(1)))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (js-object #f '()) (js-string '(OPEN o b j e c t SPACE O b j e c t SHUT)))
  (js-boolean #t))

(unsafe!unit-test
  (op-=== (js-object #f '()) (js-string '(OPEN o b j e c t SPACE O b j e c t SHUT)))
  (js-boolean #f))

(unsafe!unit-test
  (op-! (js-object #f '()))
  (js-boolean #f))

(unsafe!unit-test
  (op-?: (js-object #f '()) (js-string '(t r u t h y)) (js-string '(f a l s e y)))
  (js-string '(t r u t h y)))

(unsafe!unit-test
  (op-=== (js-object #f '()) (js-object #f '()))
  (js-boolean #f))

(unsafe!unit-test
  (op-== (js-object #f '()) (js-object #f '()))
  (js-boolean #f))

(unsafe!unit-test
  (op-== (op-! (js-object #t '())) (js-object #t '()))
  (js-boolean #t))

(unsafe!unit-test
  (op-?: (js-object #t '()) (js-number 1) (js-number 2))
  (js-number 1))

(unsafe!unit-test
  (op-== (js-object #t (list (js-number 1) (js-null) (js-number 3)))
         (js-string '(1 COMMA COMMA 3)))
  (js-boolean #t))

(unsafe!unit-test
  (op-+ (js-string '(1 2 3))
        (js-string '(4 5 6)))
  (js-string '(1 2 3 4 5 6)))

(unsafe!unit-test
  (op-+ (js-number 123)
        (js-string '(4 5 6)))
  (js-string '(1 2 3 4 5 6)))

(unsafe!unit-test
  (op-+ (js-number 123)
        (js-number 456))
  (js-number 579))

(unsafe!unit-test
  (op-typeof (js-object #t '()))
  (js-string '(o b j e c t)))

(unsafe!unit-test
  (op-+ (js-object #t '())
        (js-object #t '()))
  (js-string '()))

(unsafe!unit-test
  (op-+ (js-object #t '())
        (js-object #f '()))
  (js-string '(OPEN o b j e c t SPACE O b j e c t SHUT)))

(unsafe!unit-test
  (op-+ (js-object #f '())
        (js-object #f '()))
  (js-string '(OPEN o b j e c t SPACE O b j e c t SHUT
               OPEN o b j e c t SPACE O b j e c t SHUT)))

(unsafe!unit-test
  (op-== (js-object #t '())
         (js-boolean #t))
  (js-boolean #f))

(unsafe!unit-test
  (op-== (op-! (js-object #t '()))
         (js-boolean #t))
  (js-boolean #f))

(unsafe!unit-test
  (op-== (js-object #t '())
         (js-boolean #f))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (op-! (js-object #t '()))
         (js-boolean #f))
  (js-boolean #t))

(unsafe!unit-test
  (op-+ (js-object #t (list (js-number 1) (js-number 2)))
        (js-object #t (list (js-number 3) (js-number 4))))
  (js-string '(1 COMMA 2 3 COMMA 4)))

(unsafe!unit-test
  (op-+un (js-object #t '()))
  (js-number 0))

(unsafe!unit-test
  (op-+un (js-object #f '()))
  (js-number 'NaN))

(unsafe!unit-test
  (op-== (js-null) (js-number 0))
  (js-boolean #f))

(unsafe!unit-test
  (op-< (js-string '(a b c)) (js-string '(a b c)))
  (js-boolean #f))

(unsafe!unit-test
  (op-< (js-string '(a b c)) (js-string '(a b c d)))
  (js-boolean #t))

(unsafe!unit-test
  (op-< (js-string '(b b c)) (js-string '(a b c d)))
  (js-boolean #f))

(unsafe!unit-test
  (op-< (js-string '(1)) (js-number 2))
  (js-boolean #t))

(unsafe!unit-test
  (op-< (js-string '(1 2)) (js-string '(3)))
  (js-boolean #t))

(unsafe!unit-test
  (op-< (js-null) (js-undefined))
  (js-boolean #f))

(unsafe!unit-test
  (op-< (js-object #f '()) (js-string '(a b c)))
  (js-boolean #t))

(unsafe!unit-test
  (op-< (js-object #t '()) (js-object #f '()))
  (js-boolean #t))

(unsafe!unit-test
  (op->= (js-number 3) (js-number 3))
  (js-boolean #t))

(unsafe!unit-test
  (op->= (js-number 4) (js-number 3))
  (js-boolean #t))

(unsafe!unit-test
  (op->= (js-number 2) (js-number 3))
  (js-boolean #f))

(unsafe!unit-test
  (op->= (js-string '(a b)) (js-string '(a b)))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (js-null) (js-number 0))
  (js-boolean #f))

(unsafe!unit-test
  (op-< (js-number 0) (js-null))
  (js-boolean #f))

(unsafe!unit-test
  (op->= (js-null) (js-number 0))
  (js-boolean #t))

(unsafe!unit-test
  (op-== (js-object #t (list (js-null) (js-undefined))) (js-string '(COMMA)))
  (js-boolean #t))

(unsafe!unit-test
  (op--un (js-number 3))
  (js-number -3))

(unsafe!unit-test
  (op--un (js-object #f '()))
  (js-number 'NaN))

(unsafe!unit-test
  (op--un (js-object #t '()))
  (js-number 0))

(unsafe!unit-test
  (op-- (js-number 3) (js-number 2))
  (js-number 1))

(unsafe!unit-test
  (op-- (js-number 3) (js-string '(2)))
  (js-number 1))

(unsafe!unit-test
  (op-index (js-undefined) (js-number 1))
  (js-error))

(unsafe!unit-test
  (op-index (js-null) (js-number 1))
  (js-error))

(unsafe!unit-test
  (op-index (js-object #t (list (js-number 10) (js-number 11) (js-number 12))) (js-number 1))
  (js-number 11))

(unsafe!unit-test
  (op-index (js-object #t (list (js-number 10) (js-number 11) (js-number 12))) (js-string '(1)))
  (js-number 11))

(unsafe!unit-test
  (op-index (js-object #t (list (js-number 10) (js-number 11) (js-number 12))) (js-string '(0 1)))
  (js-undefined))

(unsafe!unit-test
  (op-index (js-object #t (list (js-number 10) (js-number 11) (js-number 12))) (js-string '(- 1)))
  (js-undefined))

(unsafe!unit-test
  (op-index (js-object #t (list (js-number 10) (js-number 11) (js-number 12))) (js-number -1))
  (js-undefined))

(unsafe!unit-test
  (op-index (js-object #t (list (js-number 10) (js-number 11) (js-number 12))) (js-number 3))
  (js-undefined))

(unsafe!unit-test
  (op-index (js-string '(a b c)) (js-number 1))
  (js-string '(b)))

(unsafe!unit-test
  (op-index (js-string '(a b c)) (js-object #t (list (js-number 1))))
  (js-string '(b)))

(unsafe!unit-test
  (op-index (js-string '(a b c)) (js-object #t (list (js-string '(1)))))
  (js-string '(b)))

(unsafe!unit-test
  (op-+ (js-string '()) (op-sort (js-object #t (list (js-number 6) (js-number 8) (js-number 10) (js-number 9) (js-number 7)))))
  (js-string '(1 0 COMMA 6 COMMA 7 COMMA 8 COMMA 9)))

(unsafe!unit-test
  (op-sort (js-object #t (list (js-string '()) (js-object #t '()))))
           (js-object #t (list (js-string '()) (js-object #t '()))))