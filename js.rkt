#lang rosette
(require rosette/lib/synthax)
(require rosette/lib/destruct)
(require rosette/lib/angelic)

(require racket/trace)
(current-trace-print-args (lambda (name x y z n) (printf "> ~a ~a\n" n name)))
(current-trace-print-results (lambda (name . xyz) (printf "<\n")))

;; https://github.com/denysdovhan/wtfjs#-motivation

;;;; Scope of this model:
;; https://262.ecma-international.org/11.0/
;; https://tc39.es/ecma262
;; 6.1 We only support:
;;     Undefined, Null, Boolean, String,
;;     Number (positive integers and NaN),
;;     Object (empty or Array)

;; 12.8.4 unary/binary -
;; 18.2.5 parseInt
;; 23.1.3.30.2 Array.sort
;;  --> two possible bugs, stringification and comparison should output int not bool
;; prompt() -> returns arbitrary string
;; array indexing (by 0 or by 1)
;; 10.4.3.5 string indexing (0 or 1)


(struct js-error () #:transparent)
(struct js-null () #:transparent)
(struct js-undefined () #:transparent)
(struct js-boolean (value) #:transparent)
(struct js-number (value) #:transparent)
(struct js-string (value) #:transparent)  ; primitive vs. object?
(struct js-object (is-array? elements) #:transparent)

(struct op-typeof (value) #:transparent)
(struct op-! (a) #:transparent)
(struct op-?: (c t f) #:transparent)

(struct op-=== (a b) #:transparent)
(struct op-== (a b) #:transparent)
(struct op-+ (a b) #:transparent)
(struct op-+un (a) #:transparent)
(struct op-< (a b) #:transparent)
(struct op->= (a b) #:transparent)

(define-grammar (js-expr*)
  [expr
   (choose
     (js-null)
     (js-undefined)
     (js-boolean (bool))
     (js-number (num))
     (js-string '())
     (js-object (bool) '())
     (js-object #t (list (expr)))

     ((bop) (expr) (expr))
     ((uop) (expr))
     (op-?: (expr) (expr) (expr))
     )]
  [bop (choose op-=== op-== op-+ op-+un op-< op->=)]
  [uop (choose op-typeof op-!)]
  [bool (choose #t #f)]
  [num  (choose 0 1 2 10 'NaN)]
  )

(struct mis
  (nullobj arrayobj naneq emptyarraytruthy arrstrundef arrstrnull)
  #:transparent)

(define (misinterpreter M)

(define (char->codepoint c)
  (- (length
       (member c '(
         SPACE COMMA
         0 1 2 3 4 5 6 7 8 9
         A B C D E F G H I J K L M
         N O P Q R S T U V W X Y Z
         OPEN SHUT
         a b c d e f g h i j k l m
         n o p q r s t u v w x y z
  )))))

;; 13.5.3
(define (sem-typeof a)
  (cond
    [(and (mis-nullobj M) (js-null? a)) (js-string '(n u l l))]
    [(js-null? a) (js-string '(o b j e c t))]
    [(js-undefined? a) (js-string '(u n d e f i n e d))]
    [(js-boolean? a) (js-string '(b o o l e a n))]
    [(js-number? a) (js-string '(n u m b e r))]
    [(js-string? a) (js-string '(s t r i n g))]
    [(and (mis-arrayobj M) (js-object? a) (js-object-is-array? a)) (js-string '(a r r a y))]
    [(js-object? a) (js-string '(o b j e c t))]
    ))

;; 13.5.7
(define (sem-op-! a)
  (define a+ (sem-ToBoolean a))
  (js-boolean (not (js-boolean-value a+))))

;; 13.5.7
(define (sem-op-+un a)
  (sem-ToNumber a))

;; 13.14.1
(define (sem-op-?: c t f)  ;; NOT EAGER
  (define c+ (reduce-expression c))
  (cond [(js-error? c+) c+]
        [else
         (if (js-boolean-value (sem-ToBoolean c+))
             (reduce-expression t)
             (reduce-expression f))]
             ))

;; 6.1.6.1.20
(define (sem-Number::toString n [gas 3])
  (cond [(= gas 0) '(I n f i n i t y)]
        [(equal? n 'NaN) '(N a N)]  ;; NOT ZERO, EMPTY, etc.
        [(< n 0) (cons '- (sem-Number::toString (- n) (- gas 1)))]
        [(< n 10) (list n)]
        [else (append (sem-Number::toString (quotient n 10) (- gas 1)) (list (remainder n 10)))]
        ))

;; 6.1.6.1.13
(define (sem-Number::equal a b)
  (cond [(and (mis-naneq M) (equal? a 'NaN) (equal? b 'NaN)) #t]
        [(equal? a 'NaN) #f]
        [(equal? b 'NaN) #f]
        [else (= a b)]
        ))

;; 7.1.2
(define (sem-ToBoolean a)
  (cond
    [(js-boolean? a) a]
    [(js-undefined? a) (js-boolean #f)]
    [(js-null? a) (js-boolean #f)]
    [(and (js-number? a) (equal? (js-number-value a) 0)) (js-boolean #f)]
    [(and (js-number? a) (equal? (js-number-value a) 'NaN)) (js-boolean #f)]
    [(and (js-string? a) (equal? (js-string-value a) '())) (js-boolean #f)]

    [(and (mis-emptyarraytruthy M)
          (js-object? a)
          (js-object-is-array? a)
          (empty? (js-object-elements a))) (js-boolean #f)]
    [else (js-boolean #t)]
    ))


;; 23.1.3.18
(define (sem-Array::toString-stringify x)
  (cond
    [(and (mis-arrstrundef M) (js-undefined? x)) (js-string '(u n d e f i n e d))]
    [(and (mis-arrstrnull M) (js-null? x)) (js-string '(n u l l))]
    [(js-undefined? x) (js-string '())]
    [(js-null? x) (js-string '())]
    [else (sem-ToString x)]
    ))

(define (sem-Array::toString-intercalate xs)
  (if (empty? xs) '()
      (let [(tail (sem-Array::toString-intercalate (cdr xs)))
            (head (js-string-value (sem-Array::toString-stringify (car xs))))]
           (if (empty? (cdr xs)) head (append head '(COMMA) tail)))))

(define (sem-Array::toString arr)
  ;; NOT square brackets around the output
  (js-string (sem-Array::toString-intercalate (js-object-elements arr))))

;; 20.1.3.6
(define (sem-Object::toString obj)
  (if (js-object-is-array? obj)
    (sem-Array::toString obj)
    (js-string '(OPEN o b j e c t SPACE O b j e c t SHUT))))

;; 20.1.3.7
(define (sem-Object::valueOf obj) obj)

;; 7.1.1
(define (sem-ToPrimitive a hint)
  (cond
    ;; TODO: this is hacky, fix me
    [(and (js-object? a) (equal? hint 'STRING)) (sem-Object::toString a)]
    [(and (js-object? a) (equal? hint 'NUMBER)) (sem-Object::toString a)]
    [else a]
    ))

;; 7.1.4
(define (sem-ToNumber a [final #f])
  (cond
    [(js-number? a) a]
    [(js-undefined? a) (js-number 'NaN)]
    [(js-null? a) (js-number 0)]
    [(and (js-boolean? a) (not (js-boolean-value a))) (js-number 0)]
    [(and (js-boolean? a) (js-boolean-value a)) (js-number 1)]
    [(js-string? a) (js-number (sem-StringToNumber (js-string-value a)))]
    [final (js-error)]
    [else (sem-ToNumber (sem-ToPrimitive a 'NUMBER) #t)]
    ))

;; 7.1.4.1.1
(define (sem-StringToNumber a)
  (sem-StringToNumber-helper a 0))

(define (sem-StringToNumber-helper a acc)
  (cond
    [(empty? a) acc]
    [(not (member (car a) '(0 1 2 3 4 5 6 7 8 9))) 'NaN]
    [else (sem-StringToNumber-helper (cdr a) (+ (* acc 10) (car a)))]
    ;; NOT octal if 0-prefixed
    ;; NOT empty -> NaN!
    ;; TODO: handle negative sign prefix
    ))

;; 7.1.17
(define (sem-ToString a [gas 1])
  (cond
    [(< gas 0) (js-error)]
    [(js-string? a) a]
    [(js-undefined? a) (js-string '(u n d e f i n e d))]
    [(js-null? a) (js-string '(n u l l))]
    [(js-boolean? a) (if (js-boolean-value a) (js-string '(t r u e)) (js-string '(f a l s e)))]
    [(js-number? a) (js-string (sem-Number::toString (js-number-value a)))]
    [else (sem-ToString (sem-ToPrimitive a 'STRING) (- gas 1))]
    ))

;; 7.2.15
(define (sem-op-=== a b)
  (cond
    [(not (equal? (sem-typeof a) (sem-typeof b)))
     (js-boolean #f)]
    [(js-object? a) (js-boolean #f)]  ; references not implemented
    [(js-number? a)
     (js-boolean (sem-Number::equal (js-number-value a) (js-number-value b)))]
    ;; 7.2.12
    [(js-null? a) (js-boolean #t)]
    [(js-undefined? a) (js-boolean #t)]
    [else (js-boolean (equal? a b))]
    ))

;; 7.2.14
(define (sem-op-== x y [gas 4])
  (cond
    ;; NOT just strict ===
    [(= gas 0) (js-error)]
    [(equal? (sem-typeof x) (sem-typeof y))
     (sem-op-=== x y)]
    [(and (js-null? x) (js-undefined? y)) (js-boolean #t)]
    [(and (js-undefined? x) (js-null? y)) (js-boolean #t)]

    [(and (js-number? x) (js-string? y))
     (sem-op-== x (sem-ToNumber y) (- gas 1))]
    [(and (js-string? x) (js-number? y))
     (sem-op-== (sem-ToNumber x) y (- gas 1))]
     ;; NOT coerces other side to bools
    [(js-boolean? x)
     (sem-op-== (sem-ToNumber x) y (- gas 1))]
    [(js-boolean? y)
     (sem-op-== x (sem-ToNumber y) (- gas 1))]
    [(and (js-object? y) (not (js-object? x)))
     (sem-op-== x (sem-ToPrimitive y 'NUMBER) (- gas 1))]
    [(and (js-object? x) (not (js-object? y)))
     (sem-op-== (sem-ToPrimitive x 'NUMBER) y (- gas 1))]
    [else (js-boolean #f)]
    ))

;; 13.8.1 -> 13.15.3
(define (sem-op-+ x y)
  ;; NOT works for arrays
  (let [(lPrim (sem-ToPrimitive x 'NUMBER))
        (rPrim (sem-ToPrimitive y 'NUMBER))]
       (if (or (js-string? lPrim) (js-string? rPrim))
           (js-string
               (append (js-string-value (sem-ToString lPrim))
                       (js-string-value (sem-ToString rPrim))))
           (let [(lNum (sem-ToNumber lPrim))
                 (rNum (sem-ToNumber rPrim))]
                (if (or (equal? 'NaN (js-number-value lPrim))
                        (equal? 'NaN (js-number-value rPrim)))
                    (js-number 'NaN)
                    (js-number (+ (js-number-value lPrim)
                                  (js-number-value rPrim))))))))

;; 6.1.6.1.12
(define (sem-Number::lessThan x y)
  (cond
    ;; NOT nan is less/greater than everything
    [(equal? x 'NaN) #f]
    [(equal? y 'NaN) #f]
    [else (< x y)]
    ))

(define (sem-String::lessThan x y)
  (cond
    [(empty? y) #f]
    [(empty? x) #t]
    [else
     (let [(cx (char->codepoint (car x)))
           (cy (char->codepoint (car y)))]
          (cond [(< cx cy) #t]
                [(> cx cy) #f]
                [else (sem-String::lessThan (cdr x) (cdr y))]
                ))]
    ))

;; 13.10.1 -> 7.2.13
(define (sem-op-< x y)
  ;; NOT casts strings to number
  (let [(px (sem-ToPrimitive x 'NUMBER))
        (py (sem-ToPrimitive y 'NUMBER))]
       (if (and (js-string? px) (js-string? py))
           (js-boolean (sem-String::lessThan (js-string-value px)
                                             (js-string-value py)))
           (js-boolean (sem-Number::lessThan (js-number-value (sem-ToNumber px))
                                             (js-number-value (sem-ToNumber py))))
           )))

(define (sem-op->= x y)
  ;; NOT the logical OR of == and !<
  (let [(imm (sem-op-< x y))]
    (cond [(equal? imm (js-boolean #t)) (js-boolean #f)]
          [else (js-boolean #t)]
          )))

(define (reduce-unop op a)
  (let* [(a+ (reduce-expression a))]
    (if (js-error? a+) a+ (op a+))))

(define (reduce-binop op a b)
  (let* [(a+ (reduce-expression a))
         (b+ (reduce-expression b))]
    (if (or (js-error? a+) (js-error? b+))
        (js-error)
        (op a+ b+))))

(define (reduce-expression e)
  (destruct e
    [(js-error) e]
    [(js-null) e]
    [(js-undefined) e]
    [(js-boolean _) e]
    [(js-number _) e]
    [(js-string _) e]
    [(js-object _ _) e] ;; TODO: evaluate args blech

    [(op-typeof a) (reduce-unop sem-typeof a)]
    [(op-! a) (reduce-unop sem-op-! a)]
    [(op-?: c t f) (sem-op-?: c t f)]
    [(op-=== a b) (reduce-binop sem-op-=== a b)]
    [(op-==  a b) (reduce-binop sem-op-==  a b)]
    [(op-+ a b) (reduce-binop sem-op-+ a b)]
    [(op-< a b) (reduce-binop sem-op-< a b)]
    [(op->= a b) (reduce-binop sem-op->= a b)]
    [(op-+un a) (reduce-unop sem-op-+un a)]
    ))

#;(trace
  sem-typeof
  sem-op-!
  sem-op-+un
  sem-op-?:
  sem-Number::toString
  sem-Number::equal
  sem-ToBoolean
  sem-Array::toString-stringify
  sem-Array::toString-intercalate
  sem-Array::toString
  sem-Object::toString
  sem-Object::valueOf
  sem-ToPrimitive
  sem-ToNumber
  sem-StringToNumber
  sem-StringToNumber-helper
  sem-ToString
  sem-op-===
  sem-op-==
  sem-op-+
  sem-Number::lessThan
  sem-String::lessThan
  sem-op-<
  sem-op->=)

reduce-expression
)

(define M-standard (mis #f #f #f #f #f #f))
(define reduce-expression (misinterpreter M-standard))


#|
(displayln "Build")
(define p (js-expr* #:depth 2))
(displayln "Mis 1")
(define r1 ((misinterpreter (mis #f #f #f #f #t #f)) p))
(displayln "Mis 2")
(define r2 ((misinterpreter (mis #f #f #f #f #f #f)) p))
(displayln "Asserting")
(assert (not (equal? r1 r2)))
(displayln "Solving")
(define sol (solve #t))
(displayln (evaluate p sol))
|#

(provide (all-defined-out))