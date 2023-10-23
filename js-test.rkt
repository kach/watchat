#lang rosette

(require rosette/lib/synthax)
(require rosette/lib/destruct)
(require rosette/lib/angelic)
(require "js.rkt")

(require (prefix-in unsafe! racket/base))
(require (prefix-in unsafe! racket/trace))

(define (unsafe!char->string c)
  (cond
    [(equal? c 'SPACE) " "]
    [(equal? c 'OPEN) "["]
    [(equal? c 'SHUT) "]"]
    [(equal? c 'COMMA) ","]
    [(symbol? c) (unsafe!symbol->string c)]
    [(number? c) (unsafe!number->string c)]
    ))

(define (unsafe!comma-splice xs)
  (cond
    [(empty? xs) '()]
    [(empty? (cdr xs)) xs]
    [else (cons (car xs) (cons ", " (unsafe!comma-splice (cdr xs))))]
    ))

(define (unsafe!js->string a)
  (destruct a
    [(js-error) "TypeError"]
    [(js-null) "null"]
    [(js-undefined) "undefined"]
    [(js-boolean b) (if b "true" "false")]
    [(js-number n)
     (if (equal? n 'NaN) "NaN" (unsafe!number->string n))]
    [(js-string s)
     (string-append "\"" (apply string-append (map unsafe!char->string s)) "\"")]
    [(js-object a f)
     (if (not a) "{}"
         (string-append
           "["
           (apply string-append (unsafe!comma-splice (map unsafe!js->string f)))
           "]"))]
    [(op-typeof a) (string-append "typeof(" (unsafe!js->string a) ")")]
    [(op-=== a b) (string-append (unsafe!js->string a) " === " (unsafe!js->string b))]
    [(op-== a b) (string-append (unsafe!js->string a) " == " (unsafe!js->string b))]
    [(op-+ a b) (string-append (unsafe!js->string a) " + " (unsafe!js->string b))]
    [(op-< a b) (string-append (unsafe!js->string a) " < " (unsafe!js->string b))]
    [(op->= a b) (string-append (unsafe!js->string a) " >= " (unsafe!js->string b))]
    [(op-+un a) (string-append "+" (unsafe!js->string a))]
    [(op-! a) (string-append "!" (unsafe!js->string a))]
    [(op-?: c t f)
     (string-append
       (unsafe!js->string c) " ? "
       (unsafe!js->string t) " : "
       (unsafe!js->string f))]
    ))

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
  (js-string '(OPEN o b j e c t SPACE O b j e c t SHUT OPEN o b j e c t SPACE O b j e c t SHUT)))

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