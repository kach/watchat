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
;;     Undefined, Null, Boolean, String (not String objects though),
;;     Number (positive integers and NaN), Object (empty or Array)

;;;; TODO:
;; 18.2.5 parseInt
;; 13.13.1 boolean &&, ||
;; 23.1.3.30.2 Array.sort comparison function
;; prompt() -> for I/O, returns arbitrary string
;; operator precedence??

;; 6.1
(struct js-error () #:transparent)
(struct js-null () #:transparent)
(struct js-undefined () #:transparent)
(struct js-boolean (value) #:transparent)
(struct js-number (value) #:transparent)
(struct js-string (value) #:transparent)
(struct js-object (is-array? elements) #:transparent)

(struct op-typeof (value) #:transparent)
(struct op-! (a) #:transparent)
(struct op-?: (c t f) #:transparent)
(struct op-index (o i) #:transparent)
(struct op-sort (a) #:transparent)

(struct op-=== (a b) #:transparent)
(struct op-== (a b) #:transparent)
(struct op-+ (a b) #:transparent)
(struct op-- (a b) #:transparent)
(struct op-+un (a) #:transparent)
(struct op--un (a) #:transparent)
(struct op-< (a b) #:transparent)
(struct op->= (a b) #:transparent)

(struct op-&& (a b) #:transparent)
(struct op-|| (a b) #:transparent)
(struct op-?? (a b) #:transparent)

(struct op-pair (a b) #:transparent)

(struct op-do (op a b c) #:transparent)
(define (op-doify expr)
  (destruct expr
    [(op-typeof a) (op-do op-typeof a (js-undefined) (js-undefined))]
    [(op-+un a) (op-do op-+un a (js-undefined) (js-undefined))]
    [(op--un a) (op-do op--un a (js-undefined) (js-undefined))]
    [(op-! a) (op-do op-! a (js-undefined) (js-undefined))]
    [(op-sort o) (op-do op-sort o (js-undefined) (js-undefined))]

    [(op-=== a b) (op-do op-=== a b (js-undefined))]
    [(op-==  a b) (op-do op-== a b (js-undefined))]
    [(op-+   a b) (op-do op-+ a b (js-undefined))]
    [(op-- a b) (op-do op-- a b (js-undefined))]
    [(op-< a b) (op-do op-< a b (js-undefined))]
    [(op->= a b) (op-do op->= a b (js-undefined))]
    [(op-index o i) (op-do op-index o i (js-undefined))]

    [(op-?: c t f) (op-do op-?: c t f)]
    [(op-&& a b) (op-do op-&& a b (js-undefined))]
    [(op-|| a b) (op-do op-|| a b (js-undefined))]
    [(op-?? a b) (op-do op-?? a b (js-undefined))]
    ))

(define-grammar (js-expr*)
  [expr
   (choose
     (js-null) (js-undefined)
     (js-boolean (?? boolean?))
     (js-number (num))
     (js-string '())
     (js-string '(a b c))
     (js-object (?? boolean?) '())
     (op-do (op) (expr) (expr) (expr))
     )]
  [op (choose op-typeof op-+un op--un op-! op-sort
              op-=== op-== op-+ op-- op-+un op-< op->= op-index
              op-?: op-&& op-|| op-?? ; op-pair
              )]
  [num (choose 0 1 2 10 'NaN)]
  )


(define-syntax (declare-mis stx)
  (let* [(args (cdr (syntax->datum stx)))
         (names (map first args))
         (costs (map second args))
         (descs (map third args))]
  (datum->syntax stx
    `(begin
       (struct mis ,names #:transparent)
       (define mis-names (list . ,descs))
       (define mis-costs (list . ,costs)))
    )))

(declare-mis
  (nullobj 1
   "null has type 'object', not 'null'")
  (arrayobj 1
   "array has type 'object', not 'array'")
  (naneq 1
   "As a special case, NaN is never equal to NaN")
  (emptyarraytruthy 1
   "All arrays, even [], are truthy")
  (arrstrundef 1
   "undefined is printed as empty string in arrays")
  (arrstrnull 1
   "null is printed as empty string in arrays")
  (nanstrempty 1
   "NaN prints as the string 'NaN', not empty")
  (nanstrzero 1
   "NaN prints as the string 'NaN', not zero")
  (obj0 1
   "{} is NaN, not 0, when casted to numbers.")
  (oneindexed 1
   "JavaScript is 0-indexed, not 1-indexed.")
  (undef0 1
   "undefined casts to NaN, not 0")
  (nullnan 1
   "null casts to 0, not NaN")
  (sortraw 1
   "Array.prototype.sort() casts elements (even numbers) to string and compares lexicographically")
  (??false 1
   "?? does not treat false as nullish")
  (??nan 1
   "?? does not treat nan as nullish")
)


(define (make-list n v)
  (if (= n 0) '()
      (cons v (make-list (- n 1) v))))

(define (make-symlist* n type?)
  (if (= n 0) '()
      (begin
        (define-symbolic* x* type?)
        (cons x* (make-symlist* (- n 1) type?)))))

(define M-standard
  (apply mis (make-list (length mis-names) #f)))

(define (make-mis*)
  (apply mis (make-symlist* (length mis-names) boolean?)))

(define (mis-cost M)
  (apply + (map (lambda (x) (if x 1 0))
                (cdr (vector->list (struct->vector M))))))



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

;; 13.5.4
(define (sem-op-+un a)
  (sem-ToNumber a))

;; 13.5.5
(define (sem-op--un a)
  (let [(n (sem-ToNumber (sem-ToPrimitive a 'NUMBER)))]
       (if (equal? (js-number-value n) 'NaN) n
           (js-number (- (js-number-value n))))))

;; 13.14.1
(define (sem-op-?: c t f)  ;; NOT EAGER
  (define c+ (reduce-expression c))
  (cond [(js-error? c+) c+]
        [else
         (if (js-boolean-value (sem-ToBoolean c+))
             (reduce-expression t)
             (reduce-expression f))]
             ))

;; 13.13.1
(define (sem-op-&& a b)
  (define a+ (reduce-expression a))
  (cond [(js-error? a+) a+]
        [(not (js-boolean-value (sem-ToBoolean a+))) a+]
        [else (reduce-expression b)]))

(define (sem-op-|| a b)
  (define a+ (reduce-expression a))
  (cond [(js-error? a+) a+]
        [(js-boolean-value (sem-ToBoolean a+)) a+]
        [else (reduce-expression b)]))

(define (sem-op-?? a b)
  (define a+ (reduce-expression a))
  (cond [(js-error? a+) a+]
        [(or (js-null? a+)
             (js-undefined? a+)
             (and (mis-??false M)
                  (js-boolean? a+)
                  (not (js-boolean-value a+)))
             (and (mis-??nan M)
                  (js-number? a+)
                  (equal? (js-number-value a+) 'NaN)))
         (reduce-expression b)]
        [else a+]))

;; 7.1.19 (sem-ToPropertyKey i)
(define (sem-ToPropertyKey i)
  (sem-ToString (sem-ToPrimitive i 'STRING)))

;; 7.1.21
(define (sem-CanonicalNumericIndexString s)
  (if (equal? (js-string-value s) '(- 0))
      (js-number 0)
      (let [(n (sem-ToNumber s))]
           (if (equal? (sem-ToString n) s)
               n (js-undefined)))))

;; 6.1.7
(define (sem-IsIntegerIndex s)
  (let [(n (sem-CanonicalNumericIndexString s))]
       (cond
         [(js-undefined? n) #f]
         [(equal? (js-number-value n) 'NaN) #f]
         ;; NOT allows negative indexing
         [(< (js-number-value n) 0) #f]
         [(mis-oneindexed M) (- (js-number-value n) 1)]
         [else (js-number-value n)]
         )))

;; 7.1.18 (ToObject)
;; 13.3.3 (reference record), 6.2.5.5 (GetValue)

(define (sem-op-index o i)
  (let* [(k (sem-ToPropertyKey i))
         (n (sem-IsIntegerIndex k))]
    (cond
      [(js-undefined? o) (js-error)]
      [(js-null? o) (js-error)]
      [(js-number? o) (js-undefined)]
      [(js-boolean? o) (js-undefined)]

      [(and (js-object? o)
            (not (js-object-is-array? o))) (js-undefined)]

      ;; 10.4.2 Array exotic objects
      [(and (js-object? o) (js-object-is-array? o))
       (if (and n (< n (length (js-object-elements o))))
           (list-ref (js-object-elements o) n)
           (js-undefined))]

      ;; 10.4.3.5 String exotic objects
      [(js-string? o)
       (if (and n (< n (length (js-string-value o))))
           (js-string (list (list-ref (js-string-value o) n)))
           (js-undefined))]

      [else (js-undefined)]
      )))

;; 6.1.6.1.20
(define (sem-Number::toString n [gas 3])
  (cond [(= gas 0) '(I n f i n i t y)]
        [(and (mis-nanstrempty M) (equal? n 'NaN)) '()]
        [(and (mis-nanstrzero M) (equal? n 'NaN)) '(0)]
        [(equal? n 'NaN) '(N a N)]
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
    ; NOT bool via ToString, e.g. bool([null]) might be false
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

    [(and (mis-undef0 M) (js-undefined? a)) (js-number 0)]
    [(js-undefined? a) (js-number 'NaN)]

    [(and (mis-nullnan M) (js-null? a)) (js-number 'NaN)]
    [(js-null? a) (js-number 0)]

    [(and (js-boolean? a) (not (js-boolean-value a))) (js-number 0)]
    [(and (js-boolean? a) (js-boolean-value a)) (js-number 1)]
    [(js-string? a) (js-number (sem-StringToNumber (js-string-value a)))]
    [(and (mis-obj0 M) (js-object? a) (not (js-object-is-array? a))) (js-number 0)]
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

;; 13.8.2 -> 13.15.3
(define (sem-op-- x y)
  (let [(lNum (sem-ToNumber (sem-ToPrimitive x 'NUMBER)))
        (rNum (sem-ToNumber (sem-ToPrimitive y 'NUMBER)))]
       (if (or (equal? 'NaN (js-number-value lNum))
               (equal? 'NaN (js-number-value rNum)))
           (js-number 'NaN)
           (js-number (- (js-number-value lNum)
                         (js-number-value rNum))))))

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


;; 23.1.3.30.2
(define (sem-CompareArrayElements x y)
; (printf "~a ~a\n" x y)
  (cond
    [(and (js-undefined? x) (js-undefined? y)) (js-number 0)]
    [(js-undefined? x) (js-number 1)]
    [(js-undefined? y) (js-number -1)]
    ; TODO: comparefn
    [else ;; NOT doesn't cast to string
     (let [(xString (if (mis-sortraw M) x (sem-ToString x)))
           (yString (if (mis-sortraw M) y (sem-ToString y)))]
          (cond
            [(js-boolean-value (sem-op-< xString yString))
             (js-number -1)]
            [(js-boolean-value (sem-op-< yString xString))
             (js-number 1)]
            [else (js-number 0)]
            ))]))

;; (c) 2023 William Alexander Brandon (WAB) (dictated orally)
(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(>= 0 (js-number-value
            (sem-CompareArrayElements (car l1) (car l2))))
     (cons (car l1) (merge (cdr l1) l2))]
    [else
     (cons (car l2) (merge l1 (cdr l2)))]
     ))

(define (mergesort l)
  (let [(len (length l))]
       (if (<= len 1) l
           (let-values
             [((head tail) (split-at l 1))]
             (merge (mergesort head) (mergesort tail))))))

(define (sem-Array.prototype.sort a)
  (cond
    [(and (js-object? a) (js-object-is-array? a))
     (js-object #t (mergesort (js-object-elements a)))]
    ; NOT can sort strings?
    [else (js-error)]
    ))

(define (reduce-unop op a)
  (let* [(a+ (reduce-expression a))]
    (if (js-error? a+) a+ (op a+))))

(define (reduce-binop op a b)
  (let* [(a+ (reduce-expression a))
         (b+ (reduce-expression b))]
    (if (or (js-error? a+) (js-error? b+))
        (js-error)
        (op a+ b+))))

(define (apply-op op a+ b+ c+)
  (cond
    [(equal? op op-typeof) (sem-typeof a+)]
    [(equal? op op-!) (sem-op-! a+)]
    [(equal? op op-index) (sem-op-index a+ b+)]
    [(equal? op op-sort) (sem-Array.prototype.sort a+)]
    [(equal? op op-===) (sem-op-=== a+ b+)]
    [(equal? op op-== ) (sem-op-==  a+ b+)]
    [(equal? op op-+) (sem-op-+ a+ b+)]
    [(equal? op op--) (sem-op-- a+ b+)]
    [(equal? op op-<) (sem-op-< a+ b+)]
    [(equal? op op->=) (sem-op->= a+ b+)]
    [(equal? op op-+un) (sem-op-+un a+)]
    [(equal? op op--un) (sem-op--un a+)]
    [(equal? op op-pair) (js-object #t (list a+ b+))]
    ))

(define (reduce-expression e)
  (destruct e
    [(js-error) e]
    [(js-null) e]
    [(js-undefined) e]
    [(js-boolean _) e]
    [(js-number _) e]
    [(js-string _) e]
    [(js-object _ _) e] ;; TODO: evaluate args blech

    [(op-do op a b c)
     (cond
       [(equal? op op-?:)
        (sem-op-?: a b c)]
       [(equal? op op-&&)
        (sem-op-&& a b)]
       [(equal? op op-||)
        (sem-op-|| a b)]
       [(equal? op op-??)
        (sem-op-?? a b)]

       [else
        (let [(a+ (reduce-expression a))]
             (if (js-error? a+) (js-error)
                 (let [(b+ (reduce-expression b))]
                      (if (js-error? b+) (js-error)
                          (let [(c+ (reduce-expression c))]
                               (if (js-error? c+) (js-error)
                                   (apply-op op a+ b+ c+)
                                   ))))))]
       )]
    [_ (reduce-expression (op-doify e))]
    ))

#;(trace
; sem-typeof
; sem-op-!
; sem-op-+un
; sem-op--un
; sem-op-?:
; sem-op-&&
; sem-op-||
; sem-op-??
; sem-ToPropertyKey
; sem-CanonicalNumericIndexString
; sem-IsIntegerIndex
; sem-op-index
; sem-CompareArrayElements
; sem-Array.prototype.sort
; mergesort merge my-split-at
; sem-Number::toString
; sem-Number::equal
; sem-ToBoolean
; sem-Array::toString-stringify
; sem-Array::toString-intercalate
; sem-Array::toString
; sem-Object::toString
; sem-Object::valueOf
; sem-ToPrimitive
; sem-ToNumber
; sem-StringToNumber
; sem-StringToNumber-helper
; sem-ToString
; sem-op-===
; sem-op-==
; sem-op-+
; sem-op--
; sem-Number::lessThan
; sem-String::lessThan
; sem-op-<
; sem-op->=
)

reduce-expression
)


(require (prefix-in unsafe! racket/base))

(define (unsafe!char->string c)
  (cond
    [(equal? c 'SPACE) " "]
    [(equal? c 'OPEN) "["]
    [(equal? c 'SHUT) "]"]
    [(equal? c 'COMMA) ","]
    [(symbol? c) (unsafe!symbol->string c)]
    [(number? c) (unsafe!number->string c)]
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
           (apply string-append (add-between (map unsafe!js->string f) ", "))
           "]"))]

    [(op-do op a b c)
     (cond
       [(equal? op op-typeof)
        (format "typeof(~a)" (unsafe!js->string a))]
       [(equal? op op-+un)
        (format "(+~a)" (unsafe!js->string a))]
       [(equal? op op--un)
        (format "(-~a)" (unsafe!js->string a))]
       [(equal? op op-!)
        (format "(!~a)" (unsafe!js->string a))]
       [(equal? op op-sort)
        (format "~a.sort()" (unsafe!js->string a))]
       [(equal? op op-===)
        (format "(~a === ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-==)
        (format "(~a == ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-+)
        (format "(~a + ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op--)
        (format "(~a - ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-<)
        (format "(~a < ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op->=)
        (format "(~a >= ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-index)
        (format "~a[~a]" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-?:)
        (format "(~a ? ~a : ~a)" (unsafe!js->string a) (unsafe!js->string b) (unsafe!js->string c))]
       [(equal? op op-&&)
        (format "(~a && ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-||)
        (format "(~a || ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-??)
        (format "(~a ?? ~a)" (unsafe!js->string a) (unsafe!js->string b))]
       [(equal? op op-pair)
        (format "[~a, ~a]" (unsafe!js->string a) (unsafe!js->string b))]
       [else "<unknown operator>"]
        )]
    [_ (unsafe!js->string (op-doify a))]
    ))

(provide (all-defined-out))