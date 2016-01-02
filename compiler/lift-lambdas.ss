(library (compiler lift-lambdas)
  (export 
    optimize-direct-call
    remove-anonymous-lambda
    sanitize-binding-forms)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | optimize-direct-call                                          |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (optimize-direct-call expr)                                   |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with each inline lambda removed.               |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass optimized away any inlined anonymous lambdas, removing the    |
;; | need for building a closure.                                            |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | The expression is walked any any anonymous lambdas found in call        |
;; | position are desugared into lets (as per normal).                       |
;; +-------------------------------------------------------------------------+
;; | INPUT  / OUTPUT GRAMMAR                                                 |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) body)                                        |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*) body)]*) Expr)                |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+

(define-who (optimize-direct-call expr)
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(lambda (,uv* ...) ,[body]) `(lambda (,uv* ...) ,body)]
      [(let ([,uv* ,[e*]] ...) ,[e]) `(let ([,uv* ,e*] ...) ,e)]
      [(letrec ([,name* (lambda (,uv* ...) ,[body*])] ...) ,[body])
        `(letrec ([,name* (lambda (,uv* ...) ,body*)] ...) ,body)]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [((lambda (,uv* ...) ,[body]) ,[e*] ...) 
        (if (and (list? uv*) (list? e*) (not (eq? (length uv*) (length e*))))
          (errorf who "Invalid number of arguments to procedure: ~s\n" expr))
        `(let ([,uv* ,e*] ...) ,body)]
      [(,[e*] ...) `(,e* ...)]))
  (Expr expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | remove-anonymous-lambda                                       |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (remove-anonymous-lambda expr)                                |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with anonymous lambdas removed.                | 
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass removes anonymous lambdas, performing bindings for them.      |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Anonymous lambdas are lifted into letrecs whose bodies are just the     |
;; | new name for the lambda itself.                                         |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) body)                                        |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*) body)]*) Expr)                |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar [Expr | (lambda (uvar*) body)]]*) Expr)          |
;; |          | (letrec ([uvar (lambda (uvar*) body)]*) Expr)                |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+

(define-who (remove-anonymous-lambda expr)
  (define (Bound-Expr expr)
    (match expr
      [(lambda (,uv* ...) ,[Expr -> body]) `(lambda (,uv* ...) ,body)]
      [(let ([,uv* ,[e*]] ...) ,[Expr -> e]) `(let ([,uv* ,e*] ...) ,e)]
      [,x (Expr x)]))
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(lambda (,uv* ...) ,[body]) 
        (let ([anon (unique-name 'anon)])
          `(letrec ([,anon (lambda (,uv* ...) ,body)]) ,anon))]
      [(let ([,uv* ,[Bound-Expr -> e*]] ...) ,[e]) `(let ([,uv* ,e*] ...) ,e)]
      [(letrec ([,name* (lambda (,uv* ...) ,[body*])] ...) ,[body])
        `(letrec ([,name* (lambda (,uv* ...) ,body*)] ...) ,body)]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,[e*] ...) `(,e* ...)]))
  (Expr expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | sanitize-binding-forms                                        |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (sanitize-binding-forms expr)                                 |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with binding forms sanitized.                  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass ensures that each letrec is bound to a lambda and each let is |
;; | bound to an expression (not a lambda).                                  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Any let or letrec binding is handed off to the process-bindings helper  |
;; | which appropriately partitions up the binding and values back the two   |
;; | sets of bindings.                                                       |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar [Expr | (lambda (uvar*) body)]]*) Expr)          |
;; |          | (letrec ([uvar (lambda (uvar*) body)]*) Expr)                |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*) Expr)]*) Expr)                |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+

(define-who (sanitize-binding-forms expr)
  (define (process-bindings binding*)
    (cond
      [(null? binding*) (values '() '())]
      [(uvar? (car binding*))
        (match binding*
          [(,uv (lambda ,arg* ,[Expr -> body]))
            (values `((,uv (lambda ,arg* ,body))) '())]
          [(,uv ,[Expr -> body])  (values '() `((,uv ,body)))])]
      [else 
        (let-values ([(lambdas lets) (process-bindings (cdr binding*))])
          (let ([first (car binding*)])
            (match first
              [(,uv (lambda ,arg* ,[Expr -> body]))
                (values (cons `(,uv (lambda ,arg* ,body)) lambdas) lets)]
              [(,uv ,[Expr -> body]) (values lambdas (cons `(,uv ,body) lets))])))]))
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(let (,binding* ...) ,[e]) 
        (let-values ([(lambdas lets) (process-bindings binding*)])
          (cond
            [(and (null? lambdas) (null? lets)) e]
            [(null? lambdas) `(let (,lets ...) ,e)]
            [(null? lets) `(letrec (,lambdas ...) ,e)]
            [else `(letrec (,lambdas ...) (let (,lets ...) ,e))]))]
      [(letrec (,binding* ...) ,[e]) 
        (let-values ([(lambdas lets) (process-bindings binding*)])
          (cond
            [(and (null? lambdas) (null? lets)) e]
            [(null? lambdas) `(let (,lets ...) ,e)]
            [(null? lets) `(letrec (,lambdas ...) ,e)]
            [else `(letrec (,lambdas ...) (let (,lets ...) ,e))]))]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,[e*] ...) `(,e* ...)]))
  (Expr expr))

)
