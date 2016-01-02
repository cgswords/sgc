(library (compiler purify-assignments)
  (export 
    uncover-assigned
    purify-letrec
    convert-assignments)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | uncover-assigned                                              |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (uncover-assigned expr)                                       |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with the assigned form introduced.             |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | The assigned form holds a list of all assigned variables in the         |
;; | expression. This is useful later for dealing with set!                  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Recursively walking the grammar, any assignment is valued back with the |
;; | revised expression. Any time you might introduce an assigned form, the  |
;; | variables valued back are used to build the form.                       |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Datum)                                                |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) body)                                        |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar Expr]*) Expr)                                 |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Datum   -> immediate                                                    |
;; |          | (Datum . Datum)                                              |
;; |          | #(Datum*)                                                    |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) (assigned (uvar*) body))                     |
;; |          | (let ([uvar Expr]*) (assigned (uvar*) Expr))                 |
;; |          | (letrec ([uvar Expr]*) (assigned (uvar*) Expr))              |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+

(define-who (uncover-assigned expr)
  (define (intersect a b) (intersection a b))
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) (values '() x)]
      [(quote ,i) (guard (immediate? i)) (values '() `(quote ,i))]
      [(if ,[v1 e1] ,[v2 e2] ,[v3 e3]) 
        (values (apply union `(,v1 ,v2 ,v3)) `(if ,e1 ,e2 ,e3))]
      [(begin ,[v* e*] ... ,[v e]) 
        (values (apply union `(,v* ... ,v)) `(begin ,e* ... ,e))]
      [(lambda (,uv* ...) ,[v body]) 
        (let ([lambda-v* (intersect `(,uv* ...) v)] 
              [value-v* (difference v `(,uv* ...))])
        (values value-v* `(lambda (,uv* ...) (assigned (,lambda-v* ...) ,body))))]
      [(let ([,uv* ,[v* e*]] ...) ,[v e]) 
        (let ([let-v* (intersect `(,uv* ...) v)] 
              [value-v* (apply union `(,v* ... ,(difference v `(,uv* ...))))])
        (values value-v* `(let ([,uv* ,e*] ...) (assigned (,let-v* ...) ,e))))]
      [(letrec ([,uv* ,[v* e*]] ...) ,[v e])
        (let ([let-v* (intersect `(,uv* ...) v)] 
              [value-v* (apply union `(,v* ... ,(difference v `(,uv* ...))))])
        (values value-v* `(letrec ([,uv* ,e*] ...) (assigned (,let-v* ...) ,e))))]
      [(set! ,x ,[v e]) (values (union `(,x) v) `(set! ,x ,e))]
      [(,prim ,[v* e*] ...) (guard (primitive? prim)) (values `(,v* ...) `(,prim ,e* ...))]
      [(,[v* e*] ...) (values (apply union `(,v* ...)) `(,e* ...))]))
  (let-values ([(garbage output) (Expr expr)])
    output))

;; +-------------------------------------------------------------------------+
;; | PASS    | purify-letrec                                                 |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (purify-letrec expr)                                          |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with all letrecs purified with only lambdas.   |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass walks through the grammar, ensuring that each and every bound |
;; | letrec is, in fact, associated with a lambda, lifting the rest out.     |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Recursively descending through the grammar, any letrec has its bindings |
;; | partitioned into lambdas, simple, and complex expressions. The simple   |
;; | expressions are put into a lambda outside of the letrec, the lambdas    |
;; | are left in place, adn the complex expressions are put in lets that     |
;; | necessitate set!s.                                                      |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) (assigned (uvar*) body))                     |
;; |          | (let ([uvar Expr]*) (assigned (uvar*) Expr))                 |
;; |          | (letrec ([uvar Expr]*) (assigned (uvar*) Expr))              |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) (assigned (uvar*) body))                     |
;; |          | (let ([uvar Expr]*) (assigned (uvar*) Expr))                 |
;; |          | (letrec                                                      |
;; |              ([uvar (lambda (uvar*) (assigned (uvar*) body))]*) Expr)   |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+

(define-who (purify-letrec e)
  (define (purify b* a* u* e)
    (let-values 
      ([(s* rest) (partition (lambda (b) (simple? b a* u*)) b*)]
       [(l* c*) (partition (lambda (b)
                             (let ((e (cadr b)))
                               (and (not (memq (car b) a*)) 
                                    (and (pair? e) (eq? (car e) 'lambda)))))
                           b*)])
      (let-values ([(u-c* e-c*) (unzip c*)])
          (let* ([tmp* (map (comp extract-root string->symbol unique-name) u-c*)]
                 [complex-a 
                   (if (null? u-c*) e (with-ellipsis-aware-quasiquote
                                        `(let ([,tmp* ,e-c*] ...)
                                           (assigned () (begin (set! ,u-c* ,tmp*) ...  ,e)))))]
                 [lambdas   
                   (if (null? l*) complex-a `(letrec ,l* (assigned () ,complex-a)))]
                 [complex-b 
                   (if (null? u-c*) lambdas (with-ellipsis-aware-quasiquote
                                              `(let ([,u-c* (void)] ...)
                                                 (assigned ,u-c* ,lambdas))))])
            (if (null? s*) complex-b `(let ,s* (assigned () ,complex-b)))))))
  (define (simple? binding assigned* letrec-u*)
    (let ((var (car binding)) (exp (cdr binding)))
      (match exp
        [,uvar (guard (uvar? uvar)) 
         (not (or (memq uvar letrec-u*)
              (memq uvar assigned*)))]
        [(quote ,x) #t]
        [(if ,[t] ,[c] ,[a]) (and t c a)]
        [(begin ,[e*] ... ,[e]) (ormap (lambda (x) x) (cons e e*))]
        [(let ([,u* ,[e*]] ...) (assigned ,a* ,[e]))
         (ormap (lambda (x) x) (cons e e*))]
        [(set! ,uvar ,[e]) (and e
                                (not (or (memq uvar letrec-u*)
                                         (memq uvar assigned*))))]
        [(,prim ,[e*] ...) (and (not (eq? 'call/cc prim))
                                (primitive? prim))]
        [,else #f])))
  (define (Expr exp)
    (match exp
      [,uvar (guard (uvar? uvar)) uvar]
      [(quote ,x) `',x]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(lambda (,fml* ...) (assigned ,a* ,[e]))
       `(lambda (,fml* ...) (assigned ,a* ,e))]
      [(let ([,u* ,[e*]] ...) (assigned ,a* ,[e]))
       `(let ([,u* ,e*] ...) (assigned ,a* ,e))]
      [(letrec ([,u* ,[e*]] ...) (assigned ,a* ,[e]))
       (purify `((,u* ,e*) ...) a* u* e)]
      [(set! ,uvar ,[e]) `(set! ,uvar ,e)]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,[e] ,[e*] ...) `(,e ,e* ...)]
      [,err  (error who "invalid Expr ~s" err)]))
  (Expr e))

;; +-------------------------------------------------------------------------+
;; | PASS    | convert-assignments                                           |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (convert-assignments expr)                                    |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with the assigned form gone and assignments    |
;; |         | transformed into set-car!s.                                   |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | The expression gets all of its assigned forms removed. Each variable    |
;; | that is set!ed is transformed into a cons cell and each set! becomes a  |
;; | call to set-car!                                                        |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | The grammar is walked and the appropriate transformations are performed |
;; | as they are found. Overall, it si straight-forward with the exception   |
;; | that assigned vars must be built into cons cells (which is performed by |
;; | build-value-list).                                                      | 
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (uvar*) (assigned (uvar*) body))                     |
;; |          | (let ([uvar Expr]*) (assigned (uvar*) Expr))                 |
;; |          | (letrec                                                      |
;; |              ([uvar (lambda (uvar*) (assigned (uvar*) body))]*) Expr)   |
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
;; |          | (lambda (uvar*) body)                                        |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*) body)]*) Expr)                |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+

(define-who (convert-assignments expr)
  (define (rep-vars body a*)
    (cond
      [(null? body) '()]
      [(and (pair? body) (eq? (car body) 'set-car!)) 
        (cons (car body) (cons (cadr body) (rep-vars (cddr body) a*)))]
      [(pair? body) (cons (rep-vars (car body) a*) (rep-vars (cdr body) a*))]
      [(memq body a*) `(car ,body)]
      [else body])) 
  (define (lookup-replace-lambda alist)
    (lambda (var)
      (let ([lookup (assq var alist)]) (if lookup (cdr lookup) var))))
  (define (lookup-replace-let alist)
    (lambda (var rhs)
      (let ([lookup (assq var alist)])  (if lookup `(,(cdr lookup) ,rhs) `(,var ,rhs)))))
  (define (build-inner v e) `(,v (cons ,e (void))))
  (define (build-new-binding e) (unique-name (string->symbol (extract-root e))))
  (define (build-value-list uv* rhs* a*)
    (let* ([new-bindings (map build-new-binding a*)]
           [inner-bindings (map build-inner a* new-bindings)]
           [outer-bindings (map (lookup-replace-let (zip a* new-bindings)) uv* rhs*)])
      (values outer-bindings inner-bindings)))
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(lambda (,uv* ...) (assigned (,a* ...) ,[body])) 
        (if (null? `(,a* ...))
          `(lambda (,uv* ...) ,body)
          (let* ([new-bindings (map build-new-binding `(,a* ...))]
                 [inner (map build-inner `(,a* ...) new-bindings)]     
                 [args (map (lookup-replace-lambda (zip `(,a* ...) new-bindings)) `(,uv* ...))])
            `(lambda (,args ...) (let (,inner ...) ,(rep-vars  body `(,a* ...))))))]
      [(let ([,uv* ,[e*]] ...) (assigned (,a* ...) ,[e])) 
        (if (null? `(,a* ...)) 
          `(let ([,uv* ,e*] ...) ,e)
          (let-values 
            ([(outer inner) (build-value-list `(,uv* ...) `(,e* ...) `(,a* ...))])
            `(let (,outer ...) (let (,inner ...) ,(rep-vars  e `(,a* ...))))))]
      [(letrec ([,name* ,[body*]] ...) (assigned (,a* ...) ,[body]))
       (let*-values
         ([(bodies) (map (lambda (b) (rep-vars b `(,a* ...))) `(,body* ...))]
          [(outer inner) (build-value-list `(,name* ...) bodies `(,a* ...))])
         (if (null? inner)
            `(letrec (,outer ...) ,(rep-vars  body `(,a* ...))) 
            `(letrec (,outer ...) 
               (let (,inner ...) ,(rep-vars  body `(,a* ...))))))]
      [(set! ,uv ,[e]) `(set-car! ,uv ,e)]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,[e*] ...) `(,e* ...)])) 
  (Expr expr))

)
