(library (compiler convert-closures)
  (export 
    uncover-free
    convert-closures
    optimize-known-call
    introduce-procedure-primitives)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | uncover-free                                                  |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (uncover-free expr)                                           |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The original expression with the (free (uvar*) ...) form      |
;; |         | introduced into each (lambda (uvar*) ...) expression.         |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | It is essentail to know which variables are free in the closures. So we |
;; | walk each lambda expression and find the free variables in each. Then   |
;; | we introduce the (free ...) form and list these free variables.         |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Free variables are found and returned up (using values). Any time the   |
;; | free form is added, the current free list is built appropriately and    |
;; | dropped into the correct place. Walking with the whole grammar performs |
;; | the task to completion.                                                 |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
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
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*) (free (uvar*) Expr))]*) Expr) |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+

(define-who (uncover-free expr)
  (define (fold-union ls) 
    (cond
      [(null? ls) '()]
      [(not (list? (car ls))) ls]
      [else (flatten (remove-nulls (fold-left union '() ls)))]))
  (define (Lambdas lambda* bindings)
    (cond
      [(null? lambda*) (values '() '())]
      [else
        (match (car lambda*)
          [(lambda ,arg* ,[Expr -> body body-free])
            (let-values ([(l* f*) (Lambdas (cdr lambda*) bindings)])
              (let* ([lambda-frees (difference body-free arg*)]
                     [free (difference (union (fold-union f*) lambda-frees) bindings)])
                (values 
                  (cons `(lambda ,arg* (free (,lambda-frees ...) ,body)) l*)
                  free)))])]))
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) (values x `(,x))]
      [(quote ,i) (guard (immediate? i)) (values `(quote ,i) '())]
      [(if ,[Expr -> t tf] ,[Expr -> c cf] ,[Expr -> a af])
        (values `(if ,t ,c ,a) (union tf cf af))]
      [(begin ,[Expr -> e* f*] ... ,[Expr -> e f])
        (values `(begin ,e* ... ,e) (union (fold-union f*) f))]
      [(let ([,x* ,[Expr -> e* f*]] ...) ,[Expr -> body body-free])
        (let ([frees (difference (union (fold-union f*) body-free) x*)])
          (values `(let ([,x* ,e*] ...) ,body) frees))]
      [(letrec ([,lbl* ,lambda*] ...) ,[Expr -> body body-free])
        (let-values ([(lam* free*) (Lambdas `(,lambda* ...) lbl*)])
          (values
            `(letrec ([,lbl* ,lam*] ...) ,body)
            (difference (union (fold-union free*) body-free) lbl*)))]
      [(,prim ,[Expr -> e* free*] ...) (guard (primitive? prim))
        (values `(,prim ,e* ...) (fold-union free*))]
      [(,[Expr -> e* free*] ...)
        (values `(,e* ...) (fold-union free*))]))
  (let-values ([(e f) (Expr expr)])
    e))

;; +-------------------------------------------------------------------------+
;; | PASS    | convert-closures                                              |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (convert-closures expr)                                       |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with 'closures' and 'bind-free' forms added.   |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This takes a nice walk through the grammar, dealing with free forms to  |
;; | build the lets with make-procedure, construct the closure forms, and    |
;; | introcude extra arguments to the lambdas and free forms, producing the  |
;; | bind-free form.                                                         |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | A grammar walk is performed. Each lambda is handed off to the Lambda    |
;; | helper which introduces the additional argument to lambdas and adds the |
;; | bind-free form, including this new variable and all of the ones in the  |
;; | original free form. The closures form are then built from the letrec    |
;; | forms, using the free form for length and completing the task.          |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*) (free (uvar*) Expr))]*) Expr) |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*)                               |
;; |                             (bind-free (uvar*) Expr))]*)                |
;; |                               (closures ([uvar lbl uvar*] ...) Expr))   |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+

(define-who (convert-closures expr)
  (define (Lambda lam)
    (cond
      [(null? lam) (values '() '())]
      [else
        (match lam
          [(,uv (lambda (,uv* ...) (free (,fv* ...) ,[Expr -> e])))
            (let ([new-lbl (unique-label uv)]
                  [clos-var (unique-name 'cp)])
              (values
                `[,new-lbl (lambda (,clos-var ,uv* ...) (bind-free (,clos-var ,fv* ...) ,e))]
                `[,uv ,new-lbl ,fv* ...]))]
          [,x (printf "Invalid lambda: ~s\n" x) (errorf who "Invalid lambda.")])]))
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(let ([,uv* ,[e*]] ...) ,[e]) `(let ([,uv* ,e*] ...) ,e)]
      [(letrec (,[Lambda -> lambda* clos*] ...) ,[e])
        `(letrec (,lambda* ...) (closures (,clos* ...) ,e))]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,uv ,[e*] ...) (guard (uvar? uv)) `(,uv ,uv ,e* ...)]
      [(,[rator] ,[e*] ...) 
        (let ([tmp (unique-name 'tmp)])
          `(let ([,tmp ,rator])
            (,tmp ,tmp ,e* ...)))]
      [,x (printf "Invalid expression: ~s\n" x) (errorf who "Invalid expression.")]))
  (Expr expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | optimize-known-call                                           |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (optimize-known-call expr)                                    |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with all known calls optimized so as to avoid  |
;; |         | code lookup.                                                  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Each function whose label is within scope has its code lookup call      |
;; | replaced with a call to the label itself.                               |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Tree walk + inline replacement.                                         |
;; +-------------------------------------------------------------------------+
;; | INPUT / OUTPUT GRAMMAR                                                  |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar (lambda (uvar*)                               |
;; |                             (bind-free (uvar*) Expr))]*)                |
;; |                               (closures ([uvar lbl uvar*] ...) Expr))   |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+

(define-who (optimize-known-call expr)
  (define (calls env)
    (lambda (expr)
      (match expr
        [,x (guard (atom? x)) x]
        [(lambda ,arg* ,[(calls env) -> body]) `(lambda ,arg* ,body)]
        [(,[(calls env) -> a] ,[(calls env) -> d] ...)
          (cond
            [(uvar? a) (let ([lookup (assq a env)])
                         (if lookup `(,(cdr lookup) ,d ...) `(,a ,d ...)))]
            [else `(,a ,d ...)])])))
  (define (build-env uv* lbl*)
    (cond
      [(null? uv*) '()]
      [(atom? uv*) `((,uv* . ,lbl*))]
      [else
        (cons 
          `(,(car uv*) . ,(car lbl*))
          (build-env (cdr uv*) (cdr lbl*)))]))
  (define (Expr expr)
    (match expr
      [,x (guard (or (label? x) (uvar? x))) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(let ([,uv* ,[e*]] ...) ,[e]) `(let ([,uv* ,e*] ...) ,e)]
      [(letrec 
        ([,lbl* (lambda ,arg* (bind-free ,uv* ,[body*]))] ...) 
          (closures ([,cuv* ,clbl* ,cuvs* ...] ...) ,[body]))
        (let ([env (build-env cuv* clbl*)])
          `(letrec ([,lbl* (lambda ,arg* (bind-free ,uv* ,((calls env) body*)))] ...)
            (closures ([,cuv* ,clbl* ,cuvs* ...] ...) ,((calls env) body))))]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,uv ,[e*] ...) (guard (uvar? uv)) `(,uv ,e* ...)]
      [(,[e*] ...) `(,e* ...)]
      [,x (errorf who "Unmatched expression ~s\n" x)]))   
  (Expr expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | introduce-procedure-primitives                                |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (introduce-procedure-primitives expr)                         |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The final transformation of closures; functions are now built |
;; |         | and used via calls to make-procedure, procedure-ref,          |
;; |         | procedure-set!, and procedure-code.                           |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass deals with all of the closures and bind-free forms,           |
;; | replacing them with calls to make-procedure. It also deals with all     |
;; | free variable references and bindings, converting the references to     |
;; | procedure-ref and the bindings to procedure-set!. All of the function   |
;; | calls (and code references) are converted to procedure-code calls.      |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | The grammar is walked. Any time the closures form is found, the helper  |
;; | build-frees is called, producing the appropriate procedure-set!s. The   |
;; | closures form is alsom used to create calls to make-procedure. Finally, |
;; | each closure is walked, converting all calls to free variables into     |
;; | calls to procedure-ref.                                                 |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([lbl* (lambda (uvar*)                               |
;; |                             (bind-free (uvar*) Expr))]*)                |
;; |                               (closures ([uvar lbl uvar*] ...) Expr))   |
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
;; +-------------------------------------------------------------------------+
(define-who (introduce-procedure-primitives expr)
  (define(build-frees var frees index)
    (cond
      [(null? frees) '()]
      [else
        (cons 
          `(procedure-set! ,var (quote ,index) ,(car frees))
          (build-frees var (cdr frees) (add1 index)))]))
  (define(Closure clos)
    (let* ([var (car clos)] [lbl (cadr clos)]
           [frees (cddr clos)] [fsize (length frees)])
      (values 
        `[,var (make-procedure ,lbl (quote ,fsize))]
        (build-frees var frees 0))))
  (define (conv-frees var f s) ;; f -> free, s -> size
    (lambda (expr)
      (let ([loc (memq expr f)])
        (match expr
          [,x (guard (and (uvar? x) loc)) 
            `(procedure-ref ,var (quote ,(sub1 (- s (length loc)))))]
          [,x (guard (atom? x)) x]
          [(procedure-ref ,a ,b) `(procedure-ref ,a ,b)]
          [(,[(conv-frees var f s) -> a] . ,[(conv-frees var f s) -> d]) `(,a . ,d)]))))
  (define (Expr expr)
    (match expr
      [,x (guard (or (label? x) (uvar? x))) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(,prim ,[e*] ...) (guard (primitive? prim)) `(,prim ,e* ...)]
      [(,uv ,[e*] ...) (guard (uvar? uv)) `((procedure-code ,uv) ,e* ...)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(let ([,uv* ,[e*]] ...) ,[e]) `(let ([,uv* ,e*] ...) ,e)]
      [(letrec ([,lbl* (lambda ,arg* ,[body*])] ...) ,[body])
        `(letrec ([,lbl* (lambda ,arg* ,body*)] ...) ,body)]
      [(bind-free ,frees ,[body]) 
        ((conv-frees (car frees) `(,frees ...) (length `(,frees ...))) body)] 
      [(closures (,[Closure -> clos* frees] ...) ,[body])
        (let ([free* (apply append frees)])
          `(let (,clos* ...)
              ,(make-begin `(,free* ... ,body))))]
      [(,[e*] ...) `(,e* ...)]
      [,x (errorf who "Unmatched expression ~s\n" x)]))
  (Expr expr))

)
