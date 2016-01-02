(library (compiler desugar-input)
  (export 
    parse-file
    handle-loads
    handle-define-forms
    parse-scheme
    convert-complex-datum)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (compiler helpers)
    (compiler expander))

(define (prim? e)
    (or (eq? e 'set!) (primitive? e)))

(define (var? e) (symbol? e))


;; +-------------------------------------------------------------------------+
;; | PASS    | handle-loads                                                  |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (handle-loads expr)                                           |
;; +-------------------------------------------------------------------------+
;; | RETURNS | A scheme expression with loads imported and desugared.        |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> var                                                          |
;; |          | constant                                                     |
;; |          | (quote Datum)                                                |
;; |          | (load String)                                                |
;; |          | (if Expr Expr)                                               |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (and Expr*)                                                  |
;; |          | (or Expr*)                                                   |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (var*) Expr+)                                        |
;; |          | (let ([var Expr]*) Expr+)                                    |
;; |          | (letrec ([var Expr]*) Expr+)                                 |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; |          | (letrec-syntax ([uvar Expr]*) Expr)                          |
;; |          | (syntax-rules (var*) (Pat Expr)*)                            |
;; |          | Define Expr*                                                 |
;; |          | Macro Expr*                                                  |
;; |          | Expr Expr*                                                   |
;; | Pat     -> var                                                          |
;; |          | (pat*)                                                       |
;; | Define  -> (define var Expr)                                            |
;; | Macro   -> (define-syntax name Expr)                                    |
;; | Datum   -> immediate                                                    |
;; |          | (Datum . Datum)                                              |
;; |          | #(Datum*)                                                    |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> var                                                          |
;; |          | constant                                                     |
;; |          | (quote Datum)                                                |
;; |          | (if Expr Expr)                                               |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (and Expr*)                                                  |
;; |          | (or Expr*)                                                   |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (var*) Expr+)                                        |
;; |          | (let ([var Expr]*) Expr+)                                    |
;; |          | (letrec ([var Expr]*) Expr+)                                 |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; |          | (letrec-syntax ([uvar Expr]*) Expr)                          |
;; |          | (syntax-rules (var*) (Pat Expr)*)                            |
;; |          | Define Expr*                                                 |
;; |          | Macro Expr*                                                  |
;; |          | Expr Expr*                                                   |
;; | Pat     -> var                                                          |
;; |          | (pat*)                                                       |
;; | Define  -> (define var Expr)                                            |
;; | Macro   -> (define-syntax name Expr)                                    |
;; | Datum   -> immediate                                                    |
;; |          | (Datum . Datum)                                              |
;; |          | #(Datum*)                                                    |
;; +-------------------------------------------------------------------------+
(define parse-file
  (lambda (name)
    (define read-fp
      (lambda (fp)
        (let ((expr (read fp)))
          (if (eof-object? expr) 
              '() 
              (cons expr (read-fp fp))))))
    (call-with-input-file name read-fp)))

(define-who handle-loads
  (lambda (prog)
    (match prog
      [() '()]
      [((load ,[parse-file -> s]) . ,rest) `(,s ... ,(handle-loads rest) ...)]
      [(,expr) prog]
      [(,expr . ,rest) (cons expr (handle-loads rest))])))

;; +-------------------------------------------------------------------------+
;; | PASS    | handle-top-level-expressions                                  |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (handle-top-level-expressons expr)                            |
;; +-------------------------------------------------------------------------+
;; | RETURNS | A scheme expression with 'define' removed and bound to letrec |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass walks across the input language, extracting definition forms  |
;; | into an external letrec, while simultaneously dealing with any top-     |
;; | level expressions.                                                      |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Walks the scheme expression, gathering up top-level definitions and     |
;; | top-level expressions, building the appropriate letrec as a result.     |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> var                                                          |
;; |          | constant                                                     |
;; |          | (quote Datum)                                                |
;; |          | (if Expr Expr)                                               |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (and Expr*)                                                  |
;; |          | (or Expr*)                                                   |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (var*) Expr+)                                        |
;; |          | (let ([var Expr]*) Expr+)                                    |
;; |          | (letrec ([var Expr]*) Expr+)                                 |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; |          | (letrec-syntax ([uvar Expr]*) Expr)                          |
;; |          | (syntax-rules (var*) (Pat Expr)*)                            |
;; |          | Define Expr*                                                 |
;; |          | Macro Expr*                                                  |
;; |          | Expr Expr*                                                   |
;; | Pat     -> var                                                          |
;; |          | (pat*)                                                       |
;; | Define  -> (define var Expr)                                            |
;; | Macro   -> (define-syntax name Expr)                                    |
;; | Datum   -> immediate                                                    |
;; |          | (Datum . Datum)                                              |
;; |          | #(Datum*)                                                    |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> var                                                          |
;; |          | constant                                                     |
;; |          | (quote Datum)                                                |
;; |          | (if Expr Expr)                                               |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (and Expr*)                                                  |
;; |          | (or Expr*)                                                   |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (var*) Expr+)                                        |
;; |          | (let ([var Expr]*) Expr+)                                    |
;; |          | (letrec ([var Expr]*) Expr+)                                 |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; |          | (letrec-syntax ([uvar Expr]*) Expr)                          |
;; |          | (syntax-rules (var*) (Pat Expr)*)                            |
;; | Pat     -> var                                                          |
;; |          | (pat*)                                                       |
;; | Datum   -> immediate                                                    |
;; |          | (Datum . Datum)                                              |
;; |          | #(Datum*)                                                    |
;; +-------------------------------------------------------------------------+

(define-who handle-define-forms
  (lambda (prog)
    (define defn-list '())
    (define syntax-list '())
    (trace-define (Expr* e)
      (match e
        [() '()]
        [((lambda ,args ,[(BindExpr '() '()) -> e]) . ,[es]) 
          `((lambda ,args ,((BindExpr '() '()) e)) . ,es)]
        [((let ((,x ,[(BindExpr '() '()) -> rhs]) ...) ,[(BindExpr '() '()) -> e]) . ,[es]) 
          `((let ((,x ,rhs) ...) ,e) . ,es)]
        [((letrec ((,x ,[(BindExpr '() '()) -> rhs]) ...) ,[(BindExpr '() '()) -> e]) . ,[es]) 
          `((letrec ((,x ,rhs) ...) ,e) . ,es)]
        [(,e . ,[es]) `(,e . ,es)]
        [(begin ,e . ,[es]) (make-begin `(,e . ,es))]))
    (trace-define (BindExpr defns syns)
      (trace-lambda 'be (e*)
        (match e*
          [() '()]
          [((define-syntax ,x ,e) . ,es) ((BindExpr defns (cons `(,x ,e) syns)) es)]
          [((define ,x ,e) . ,es) ((BindExpr (cons `(,x ,e) defns) syns) es)]
          [(,e . ,es) 
           (cond
             [(and (null? syns) (null? defns)) `(,e . ,(Expr* es))]
             [(null? syns) `(letrec ,defns (,e . ,(Expr* es)))]
             [(null? defns) `(letrec-syntax ,syns (,e . ,(Expr* es)))]
             [else `(letrec-syntax ,syns (letrec ,defns (,e . ,(Expr* es))))])]
          [(begin ,e . ,es)
           (cond
             [(and (null? syns) (null? defns)) (make-begin `(,e . ,(Expr* es)))]
             [(null? syns) `(letrec ,defns ,(make-begin `(,e . ,(Expr* es))))]
             [(null? defns) `(letrec-syntax ,syns ,(make-begin `(,e . ,(Expr* es))))]
             [else `(letrec-syntax ,syns (letrec ,defns ,(make-begin `(,e . ,(Expr* es)))))])])))
    (trace-define (Prog prog succ)
      (match prog
        [() succ]
        [(,e* ... (define-syntax ,x ,e)) 
         (begin
           (set! syntax-list (cons `(,x ,e) syntax-list))
           (Prog `(,e* ...) succ))]
        [(,e* ... (define ,x ,e)) 
         (begin
           (set! defn-list (cons x defn-list))
           (Prog `(,e* ...) (make-begin `((set! ,x ,(Expr* (list e)) ...) ,succ))))]
        [(,e* ... ,(Expr* x)) (Prog `(,e* ...) (make-begin `(,x ,succ)))]))
    (let ((body (match prog
                  [(,e* ... (define-syntax ,x ,e)) (Prog prog '(begin (void)))] 
                  [(,e* ... (define ,x ,e)) (Prog prog '(begin (void)))]
                  [(,e* ... ,x) 
                   (let ((succ (Expr* (list x))))
                     (if (eq? (car succ) 'begin) (Prog `(,e* ...) succ) (Prog `(,e* ...) (make-begin succ))))])))
      (cond
        [(and (null? syntax-list) (null? defn-list)) body]
        [(null? syntax-list) (list 'let (zip defn-list (map (lambda (x) '((void))) defn-list)) body)]
        [(null? defn-list) `(letrec-syntax ,syntax-list ,body)]
        [else (list 'let 
                (zip defn-list (map (lambda (x) '((void))) defn-list)) 
                `(letrec-syntax ,syntax-list ,body))]))))

;; +-------------------------------------------------------------------------+
;; | PASS    | parse-scheme                                                  |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (parse-scheme expr)                                           |
;; +-------------------------------------------------------------------------+
;; | RETURNS | A parsed scheme expresison wich vars replaced by uvars.       |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Walks the scheme expression, verifying that it is valid input and, at   |
;; | the same time, converting any variable into a unique variable.          |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Magic. Really, all primitives are build into the environment, paired    |
;; | with functions that deal with (and verify) the Scheme. Any new variable |
;; | or other form extends this environment as appropriate.                  |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> var                                                          |
;; |          | constant                                                     |
;; |          | (quote Datum)                                                |
;; |          | (if Expr Expr)                                               |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (and Expr*)                                                  |
;; |          | (or Expr*)                                                   |
;; |          | (begin Expr* Expr)                                           |
;; |          | (lambda (var*) Expr+)                                        |
;; |          | (let ([var Expr]*) Expr+)                                    |
;; |          | (letrec ([var Expr]*) Expr+)                                 |
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

(define-who (parse-scheme expr)
  (define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (errorf who "integer ~s is out of fixnum range" x)))))
  (define (datum? x)
    (or (constant? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x))))
        (string? x)
        (char? x)))
  (define (all-unique ls)
    (cond
      [(null? ls) #t]
      [(memq (car ls) (cdr ls)) #f]
      [else (all-unique (cdr ls))]))
  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (eq? . 2) (fixnum? . 1) (procedure? . 1) 
      (cons . 2) (null? . 1) (pair? . 1) (set-car! . 2) (set-cdr! . 2)
      (int->char . 1)   (char->int . 1) (char? . 1)
      (make-vector . 1) (vector? . 1) (vector-length . 1) (vector-ref . 2) (vector-set! . 3) 
      ;; (make-string . 1) (string? . 1) (string-length . 1) (string-ref . 2) (string-set! . 3)
      ;; (record-constructor . 1) (record-predicate . 1) (record-mutator . 2) (record-accessor . 2)
      (void . 0) (display . 1) (newline . 0)))
  (define (prim-helper env expr)
    (match expr
      [(,prim ,x* ...) (guard (assq prim primitives))
        (unless (= (length x*) (cdr (assq prim primitives)))
        (errorf who "too many or few arguments ~s for ~s" (length x*) prim))
        (let ([args (map (Expr env) `(,x* ...))])
          (cons prim args))]
      [,x (errorf who "Malformed Expr ~s" expr)]))
  (define (var-helper sym uv)
    (lambda (env expr)
      (match expr
        [,s (guard (eq? sym s)) uv]
        [(,s ,x* ...) (guard (eq? sym s))
          (let ([args (map (Expr env) `(,x* ...))])
            (cons uv args))]
      [,x (errorf who "Malformed Expr ~s" expr)])))
  (define (unique-name+ x)
    (unless (symbol? x)
      (errorf who "Invalid symbol ~s" x))
    (if (eq? x '+)
      (unique-name 'plus)
      (unique-name x)))
  (define (exp-and expr)
    (match expr
      [(and) '(quote #t)]
      [(and ,e) e]
      [(and ,e ,e* ...) 
        `(if ,e ,(exp-and `(and ,e* ...)) (quote #f))]))
  (define (exp-or expr)
    (match expr
      [(or) '(quote #f)]
      [(or ,e) e]
      [(or ,e ,e* ...) 
        (let ([t (unique-name 't)])
          `(let ([,t ,e]) (if ,t ,t ,(exp-or `(or ,e* ...)))))]))
  (define (extend-env* v* env)
    (let* ([uv* (map unique-name+ v*)]
           [zipped (zip v* uv*)]
           [nenv (ext-env* zipped env)])
      (values uv* nenv)))
  (define (ext-env* zipped env)
    (cond
      [(null? zipped) env]
      [else
        (let ([lhs (caar zipped)] [rhs (cdar zipped)])
          (cons `(,lhs . ,(var-helper lhs rhs))
                (ext-env* (cdr zipped) env)))]))
  (define (extend-env x a env) 
    `((,x . ,(var-helper x a)) . ,env))
  (define (apply-env y env)
    (let ([lookup (assq y env)])
      (if lookup
        (cdr lookup)
        (errorf who "Unbound variable ~s" y))))
  (define (Expr env)
    (lambda (expr)
      (match expr
        [,k (guard (constant? k)) `(quote ,k)]
        [,id (guard (symbol? id)) ((apply-env id env) env id)]
        [(,id . ,stuff) (guard (symbol? id))
          ((apply-env id env) env `(,id . ,stuff))]
        [(,x . ,stuff) (Application env `(,x . ,stuff))]
        [() (errorf who "invalid Expr ~s" expr)]
        [,x (errorf who "invalid Expr ~s" x)])))
  (define (Application env expr)
    (match expr
      [(,[(Expr env) -> e1] ,[(Expr env) -> e*] ...) `(,e1 ,e* ...)]
      [,x (errorf who "invalid Expr ~s" x)]))
  (define initial-env ;; SPACING IS ALL WRONG. IT HURTS MY SOUL, TOO.
    `(
      (quote . ,(lambda (env expr)
              (match expr 
                [(quote ,datum) 
                  (unless (datum? datum) (errorf who "invalid datum ~s" datum))
                  `(quote ,datum)]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (not . ,(lambda (env expr)
              (match expr 
                [(not ,[(Expr env) -> x])
                  `(if ,x (quote #f) (quote #t))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (if . ,(lambda (env expr)
              (match expr
                [(if ,[(Expr env) -> t] ,[(Expr env) -> c]) 
                  `(if ,t ,c (void))]
                [(if ,[(Expr env) -> t] ,[(Expr env) -> c] ,[(Expr env) -> a])
                  `(if ,t ,c ,a)]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (and . ,(lambda (env expr)
              (match expr
                [(and ,[(Expr env) -> e*] ...) (exp-and `(and ,e* ...))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (or . ,(lambda (env expr)
              (match expr
                [(or ,[(Expr env) -> e*]...) (exp-or `(or ,e* ...))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (begin . ,(lambda (env expr)
              (match expr
                [(begin ,[(Expr env) -> e*] ...) 
                  (unless (not (null? e*)) (errorf who "Invalid begin body ~s" expr))
                  (make-begin `(,e* ...))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (lambda . ,(lambda (env expr)
              (match expr
                [(lambda (,v* ...) ,e ...)
                  (unless (not (null? e)) (errorf who "Invalid lambda body ~s" expr))
                  (unless (all-unique `(,v* ...)) (errorf who "Invalid bindings ~s" expr))
                  (let-values ([(uv* nenv) (extend-env* `(,v* ...) env)])
                    (let ([bodies (map (Expr nenv) `(,e ...))])
                    `(lambda (,uv* ...) ,(make-begin bodies))))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (let . ,(lambda (env expr)
              (match expr
                [(let ([,v* ,[(Expr env) -> e*]] ...) ,e ...)
                  (unless (not (null? `(,e ...)))
                    (errorf who "Invalid let body ~s" `(,e ...)))
                  (unless (all-unique `(,v* ...)) (errorf who "Invalid bindings ~s" expr))
                  (let-values ([(uv* nenv) (extend-env* `(,v* ...) env)])
                    `(let ([,uv* ,e*] ...) 
                      ,(make-begin `(,((Expr nenv) e) ...))))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (letrec . ,(lambda (env expr)
              (match expr
                [(letrec ([,v* ,e*] ...) ,e ...)
                  (unless (all-unique `(,v* ...)) (errorf who "Invalid bindings ~s" expr))
                  (unless (not (null? e)) (errorf who "Invalid lambda body ~s" expr))
                  (let-values ([(uv* nenv) (extend-env* `(,v* ...) env)])
                    (let ([rhs `(,((Expr nenv) e*) ...)])
                      (let ([b* (build-bindings uv* rhs)])
                        `(letrec ,b* ,(make-begin `(,((Expr nenv) e) ...))))))]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (set! . ,(lambda (env expr)
              (match expr
                [(set! ,[(Expr env) -> uvar] ,[(Expr env) -> e])  
                   (unless (uvar? uvar) (errorf who "invalid set! lhs ~s" uvar))
                   `(set! ,uvar ,e)]
                [,x (errorf who "invalid Expr ~s" expr)])))
      (+ . ,prim-helper) (- . ,prim-helper) (* . ,prim-helper) 
      (<= . ,prim-helper) (< . ,prim-helper) (= . ,prim-helper)
      (>= . ,prim-helper) (> . ,prim-helper) (boolean? . ,prim-helper) 
      (car . ,prim-helper) (cdr . ,prim-helper) (cons . ,prim-helper) 
      (eq? . ,prim-helper) (fixnum? . ,prim-helper) (make-vector . ,prim-helper)
      (null? . ,prim-helper) (pair? . ,prim-helper) (procedure? . ,prim-helper) 
      (set-car! . ,prim-helper) (set-cdr! . ,prim-helper) (vector? . ,prim-helper) 
      (vector-length . ,prim-helper) (vector-ref . ,prim-helper) 
      (vector-set! . ,prim-helper) (void . ,prim-helper)
      (display . ,prim-helper) (newline . ,prim-helper)))
  (unique-name-count 0)
  ((Expr initial-env) expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | convert-complex-datum                                         |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (convert-complex-datum expr)                                  |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with complex datum parsed into reasonable,     | 
;; |         | non-complex datum.                                            |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass deals with quoted, non-immediate datums. It breaks them down  |
;; | into simple datums with calls to cons or vector-set! (depending on      |
;; | if the datum was a list or a vector).                                   |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Anything found in datum position is recursively picked apart into its   |
;; | basic form in either process-vector or process-list and built back up   |
;; | using the appropriate operator (vector-set! or cons).                   |
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
;; |          | (lambda (uvar*) body)                                        |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([uvar Expr]*) Expr)                                 |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; +-------------------------------------------------------------------------+

(define-who (convert-complex-datum expr)
  (define top-let* '())
  (define (process-list datum)
    (cond
      [(null? datum) `(quote ())]
      [(atom? datum) (process-complex-quote datum)]
      [else `(cons ,(process-list (car datum)) ,(process-list (cdr datum)))]))
  (define (build-setters vec-name i len exprs)
    (cond
      [(and (= i len) (null? exprs)) '()]
      [(or (= i len) (null? exprs))  (errorf who "Critical error: lol wut?\n")]
      [else
        (cons 
          `(vector-set! ,vec-name (quote ,i) ,(process-complex-quote (car exprs)))
          (build-setters vec-name (add1 i) len (cdr exprs)))]))
  (define (process-vector datum)
    (match datum
      [#(,a ...) 
        (let* ([vec-name (unique-name 'tmp)]
               [vec-length (length `(,a ...))]
               [setters (build-setters vec-name 0 vec-length `(,a ...))])
        `(let ([,vec-name (make-vector (quote ,vec-length))])
          (begin
             ,setters ... ,vec-name)))]
      [,x (errorf who "Invalid vector ~s\n" x)]))
  (define (process-complex-quote datum) 
    (match datum 
      [(,a . ,d) 
        (let ([tmp (unique-name 'tmp)]
              [expr (process-list datum)])
          (set! top-let* (cons `(,tmp ,expr) top-let*))
          tmp)] ;; list
      [#(,a ...) 
        (let ([tmp (unique-name 'tmp)]
              [expr (process-vector datum)])
          (set! top-let* (cons `(,tmp ,expr) top-let*))
          tmp)]
      [,x (guard (immediate? x)) `(quote ,x)]
      [,x (errorf who "Invalid argument ~s\n" x)])) ;; vector
  (define (Expr expr)
    (match expr
      [,x (guard (uvar? x)) x]
      [(quote ,i) (guard (immediate? i)) `(quote ,i)]
      [(quote ,d) (process-complex-quote d)]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
      [(lambda (,uv* ...) ,[body]) `(lambda (,uv* ...) ,body)]
      [(let ([,uv* ,[e*]] ...) ,[e]) `(let ([,uv* ,e*] ...) ,e)]
      [(letrec ([,name* ,[body*]] ...) ,[body])
        `(letrec ([,name* ,body*] ...) ,body)]
      [(,prim ,[e*] ...) (guard (prim? prim)) `(,prim ,e* ...)]
      [(,[e*] ...) `(,e* ...)]))
  (define (unroll-let expr top-let*)
    (cond
      [(null? top-let*) expr]
      [else
        `(let (,(car top-let*)) ,(unroll-let expr (cdr top-let*)))]))
  (unroll-let (Expr expr) (reverse top-let*)))

)
