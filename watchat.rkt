#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/http
         net/url-structs
         json)

(require "js.rkt")
(require "js-wat.rkt")

(define (expr->js expr)
  (match expr
    ['(js-undefined) (js-undefined)]
    ['(js-null) (js-null)]
    [`(js-boolean ,b) (js-boolean b)]
    ))

(define (char->js-char c)
  (cond
    [(char-numeric? c)
     (string->number (string c))]
    [(equal? (string c) "[") 'OPEN]
    [(equal? (string c) "]") 'SHUT]
    [(equal? (string c) "{") 'OPENCURLY]
    [(equal? (string c) "}") 'SHUTCURLY]
    [(equal? (string c) " ") 'SPACE]
    [(equal? (string c) ",") 'COMMA]
    [else (string->symbol (string c))]
    ))
(define (string->js-string s)
  (js-string (map char->js-char (string->list s))))

(define (jsexpr->js-expr expr)
  (match expr
    [(hash-table ('type "Identifier") ('name "NaN"))
     (js-number 'NaN)]
    [(hash-table ('type "Identifier") ('name "undefined"))
     (js-undefined)]
    [(hash-table ('type "Literal") ('value 'null))
     (js-null)]

    [(hash-table ('type "Literal") ('value (? number? n)))
     (js-number n)]
    [(hash-table ('type "Literal") ('value (? boolean? b)))
     (js-boolean b)]
    [(hash-table ('type "Literal") ('value (? string? s)))
     (string->js-string s)]

    [(hash-table ('type "UnaryExpression") ('operator "!") ('argument arg))
     (op-! (jsexpr->js-expr arg))]
    [(hash-table ('type "UnaryExpression") ('operator "+") ('argument arg))
     (op-+un (jsexpr->js-expr arg))]
    [(hash-table ('type "UnaryExpression") ('operator "-") ('argument arg))
     (op--un (jsexpr->js-expr arg))]
    [(hash-table ('type "UnaryExpression") ('operator "typeof") ('argument arg))
     (op-typeof (jsexpr->js-expr arg))]

    [(hash-table ('type "ConditionalExpression") ('test c) ('consequent t) ('alternate f))
     (op-?: (jsexpr->js-expr c) (jsexpr->js-expr t) (jsexpr->js-expr f))]

    [(hash-table ('type "BinaryExpression") ('operator "+") ('left l) ('right r))
     (op-+ (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "BinaryExpression") ('operator "-") ('left l) ('right r))
     (op-- (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "BinaryExpression") ('operator "<") ('left l) ('right r))
     (op-< (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "BinaryExpression") ('operator ">=") ('left l) ('right r))
     (op->= (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "BinaryExpression") ('operator "===") ('left l) ('right r))
     (op-=== (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "BinaryExpression") ('operator "==") ('left l) ('right r))
     (op-== (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "LogicalExpression") ('operator "&&") ('left l) ('right r))
     (op-&& (jsexpr->js-expr l) (jsexpr->js-expr r))]
    [(hash-table ('type "LogicalExpression") ('operator "||") ('left l) ('right r))
     (op-|| (jsexpr->js-expr l) (jsexpr->js-expr r))]

    [(hash-table ('type "ArrayExpression") ('elements elts))
     (js-object #t (map jsexpr->js-expr elts))]

    [(hash-table
       ('type "CallExpression")
       ('arguments '())
       ('callee
         (hash-table
           ('type "MemberExpression")
           ('object arg)
           ('property (hash-table ('type "Identifier") ('name "sort"))))))
     (op-sort (jsexpr->js-expr arg))]

    [(hash-table ('type "ObjectExpression") ('properties '()))
     (js-object #f '())]
    [(hash-table ('type "MemberExpression") ('object o) ('property i))
     (op-index (jsexpr->js-expr o) (jsexpr->js-expr i))]

    [_ (raise-user-error "Unsupported syntax")]
     ))

(define (do-cmd cmd js)
  (cond
    [(equal? cmd "eval")
     (unsafe!js->string ((misinterpreter M-standard) js))]
    [(equal? cmd "wat")
     (let [(explanation (explain-program js))] (begin (displayln explanation) explanation))]
     ))

(define (start req)
  (define cmd (path/param-path (list-ref (url-path (request-uri req)) 1)))

  (define value-string
    (bytes->string/utf-8
      (binding:form-value
        (bindings-assq #"name" (request-bindings/raw req)))))

  (define expr (string->jsexpr value-string))
  (with-handlers
    ([exn:fail:user? (lambda (e) (response/jsexpr "Unsupported syntax, sorry!"))])
    (begin
      (define js (jsexpr->js-expr expr))
      (define out (do-cmd cmd js))
      (response/jsexpr out)))
)

(serve/servlet start
  #:listen-ip "0.0.0.0"
  #:port 8889
  #:launch-browser? #f
  #:servlet-path "/"
  #:extra-files-paths (list ".")
  #:log-file (current-output-port)
  #:servlet-regexp #rx"^/run/.*"
)