(library (compiler expander)
  (export 
    subst
    wrap-syntax
    constant?
    wrap-syntax
    strip
    anti-mark
    mk
    lbl
    identifier?
    build-wrap
    push-marks
    do-subst
    do-ext-subst
    datum-to-syntax
    expose
    apply-subst
    apply-ext-subst
    resolve
    get-marks
    same-marks?
    special
    ext-env
    expand
    syn-parse
    splice-syntax
    build-transformer
    eval-trans
    empty-env
  )
  (import
    (except (chezscheme) identifier? expand subst)
    (framework match)
    (framework helpers)
    (except (compiler helpers) empty-env))

(define-record subst ((immutable symbol) (immutable marks) (immutable label) (immutable extensible)))
(define-record wrap  ((immutable expr)   (immutable marks) (immutable substs)))

(define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (errorf 'const "integer ~s is out of fixnum range" x)))))

(define wrap-syntax
  (lambda (expr)
    (match expr
      [,k (guard (constant? k)) k]
      [,id (guard (symbol? id)) (make-wrap id '() '())]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e] ,[e*] ...) `(begin ,e ,e* ...)]
;;    [(lambda ,args ,[e] ...) `(lambda ,(map syntax-object args) ,e ...)]
;;    [(letrec ([,[v] ,[e*]] ...) ,[e] ...) `(letrec ([,v ,e*] ...) ,e)]
;;    [(letrec-syntax ([,[v] ,[e*]] ...) ,[e] ...) `(letrec-syntax ([,v ,e*] ...) ,e)]
;;    [(let ([,[v] ,[e*]] ...) ,[e] ...) `(let ([,v ,e*] ...) ,e)]
      [(,[e] ,[e*] ...) `(,e ,e* ...)])))

(define strip
  (lambda (expr)
    (cond
      [(constant? expr) expr]
      [(symbol? expr)   expr]
      [(pair? expr) (cons (strip (car expr)) (strip (cdr expr)))]
      [(wrap? expr) (strip (wrap-expr expr))])))

(define anti-mark #f)
(define mk (lambda () (string #\m)))
(define lbl (lambda () (string #\l)))

(define (identifier? x) (or (symbol? x) (and (wrap? x) (identifier? (wrap-expr x)))))

(define build-wrap
  (lambda (expr marks substs)
    (let loop ((expr expr) (marks marks) (substs substs))
      (if (wrap? expr)
          (loop (wrap-expr expr) (append marks (wrap-marks expr)) (append substs (wrap-substs expr)))
          (make-wrap expr marks substs)))))
      
(define push-marks
  (lambda (expr mark)
    (cond
      [(not (wrap? expr)) (build-wrap expr (list mark) '(shift))]
      [else
        (let ((marks (wrap-marks expr)))
          (cond
            [(and (not (null? marks)) (not (car marks))) ;; the car is the anit-mark
             (build-wrap (wrap-expr expr) (cdr marks) (cdr (wrap-substs expr)))]
            [else 
              (build-wrap (wrap-expr expr) (cons mark marks) (cons 'shift (wrap-substs expr)))]))])))

(define do-subst
  (lambda (expr ident* label*)
    (build-wrap
      expr
      '()
      (list (make-subst
              (list->vector (map (lambda (x) (if (wrap? x) (wrap-expr x) x)) ident*))
              (list->vector (map (lambda (x) (if (wrap? x) (wrap-expr x) x)) ident*))
              (list->vector label*)
              #f)))))

(define do-ext-subst
  (lambda (expr ident* label*)
    (build-wrap
      expr
      '()
      (list (make-subst
              (box (map (lambda (x) (if (wrap? x) (wrap-expr x) x)) ident*)) ;; symbols
              (box (map (lambda (x) (if (wrap? x) (wrap-marks x) '())) ident*)) ;; marks
              (box label*)
              #t)))))

(define datum-to-syntax
  (lambda (stx-id datum)
    (if (symbol? (wrap-expr stx-id))
        (build-wrap datum (wrap-marks stx-id) (wrap-substs stx-id))
        (error 'datum-to-syntax "Bad syntax identifier: ~s" (strip stx-id)))))

(define expose
  (lambda (stx)
    (cond
      [(symbol? stx) stx]
      [(wrap? stx)
       (let ([expr (wrap-expr stx)])
         (cond
           [(symbol? expr) (build-wrap expr (wrap-marks stx) (wrap-substs stx))]
           [(list? expr) 
            (map 
              (lambda (x) (build-wrap x (wrap-marks stx) (wrap-substs stx))) 
              expr)]
           [else expr]))] ;; constant
      [else stx]))) ;; constant

(trace-define apply-subst
  (lambda (subst id marks)
    (let ([symbol* (subst-symbol subst)]
          [marks*  (subst-marks subst)]
          [label*  (subst-label subst)])
      (let ((symlen (vector-length symbol*)))
        (let loop ([idx 0])
          (cond
            [(= idx symlen) #f]
            [(and (eq? (vector-ref symbol* idx) id)
                  (same-marks? (vector-ref marks idx) marks))
             (vector-ref label* idx)]
            [else (loop (add1 idx))]))))))
 
(define apply-ext-subst
  (lambda (subst id marks)
    (let loop ([symbol* (unbox (subst-symbol subst))]
               [marks*  (unbox (subst-marks subst))]
               [label*  (unbox (subst-label subst))])
      (cond
        [(null? symbol*) #f]
        [(and (eq? (car symbol*) id)
              (same-marks? (car marks*) marks))
         (car label*)]
        [else (loop (cdr symbol*) (cdr marks*) (cdr label*))]))))

(trace-define resolve
  (lambda (ident)
    (trace-define resolve-loop
      (lambda (ident marks substs)
        (cond
          [(null? substs) ident]
          [(eq? (car substs) 'shift) 
           (resolve-loop ident (cdr marks) (cdr substs))]
          [(and (subst? (car substs))
                (subst-extensible (car substs)) 
                (apply-ext-subst (car substs) ident marks))]
          [(and (subst? (car substs)) 
                (apply-subst (car substs) ident marks))]
          [else (resolve-loop ident marks (cdr substs))])))
    (cond
      [(symbol? ident) ident]
      [(wrap? ident) 
       (resolve-loop (wrap-expr ident) 
                     (wrap-marks ident) 
                     (wrap-substs ident))]
      [else (error 'resolve "Bad identifier: ~s" ident)])))


(define get-marks
  (lambda (ident)
    (or (and (symbol? ident) '()) 
        (and (wrap? ident) (wrap-marks ident))
        (errorf 'get-marks "Bad identifier: ~s" ident))))

(define same-marks?
  (lambda (marks1 marks2)
    (and (= (length marks1) (length marks2))
         (andmap eq? marks1 marks2))))

(define special (let ([spc (string #\s)]) (lambda () spc)))

(define ext-env
  (lambda (x* a* oenv)
    (let* ((x* (if (list? x*) x* (list x*)))
           (a* (if (list? a*) a* (list a*))))
      (append (zip x* a*) oenv))))

(define apply-env
  (lambda (env y)
    (cond
      [(assq y env) => cdr]
      [else (errorf 'expand "Invalid lookup: ~s" y)])))

(define empty-env
  (let* ((core-forms '(let-syntax lambda quote quasiquote syntax if define
                       define-syntax letrec-syntax))
         (marks      (map (lambda (x) (special)) (iota (length core-forms)))))
    (ext-env core-forms marks '())))
 
;; Expand :: Exp x Env -> Exp
;; expand (e,r) =
;; case (parse e r) of
;;   (var i)            -> var (resolve i)
;;   (e1 e2)            -> ((expand e1 r) (expand e2 r))
;;   (sym e)            -> (strip e)
;;   (stx e)            -> e
;;   (\ i e)            -> (\ s (expand (subst e i s) r'))
;;                           Where r' = r, (s := Var) and s is fresh
;;   (\p i e)           -> (\ s (expand (subst e i s) r'))
;;                           Where r' = r, (s := PVar) and s is fresh
;;   (macro e)          -> (expand (mark (trans (mark e m)) m) r)
;;                           Where t = (looup r (resolve i)) and m is fresh
;;   (syn-bine i e1 e2) -> (expand (subst e2 i s) (ext-env s t r))
;;                           Where t = eval (expand e1 r) and s is fresh
;;   (rec-syn i e1 e2)  -> (expand (subst e2 i s) (ext-env s t r))
;;                           Where t = eval (expand (subst e1 i s) r) and s is fresh

(trace-define expand
  (lambda (stx env)
    (define-syntax core-form?
      (syntax-rules ()
        [(_ var name)
         (and (identifier? var) (eq? (resolve var) 'name)
              (eq? (apply-env env 'name) (special)))]
        [(_ var) (and (identifier? var) (eq? (apply-env env (resolve var)) (special)))]))
    (match (expose stx)
      [(,lamb ,var* ,expr* ...) (guard (core-form? lamb lambda))
       (let* ([var* (expose var*)]
              [new-var* (map (lambda (var) (unique-name (strip var))) var*)]
              [s* (map (lambda (_) (lbl)) var*)]
              [expr* (do-subst expr* var* s*)])
         `(lambda (,new-var* ...) ,(expand expr* (ext-env s* new-var* env))))]
      [(,q ,stx) (guard (core-form? q quote)) `(quote ,(strip stx))]
      [(,syn ,stx) (guard (core-form? syn syntax)) stx]
      [(,qq ,stx) (guard (core-form? qq quasiquote))
       (let recur ([stx stx] [level 0])
         (match stx
           [(,unq ,datum) (guard (core-form? unq unquote))
            (if (zero? level) 
                (expand datum env)
                (recur datum (sub1 level)))]
           [(,[a] ,[d*] ...) `(list ,a ,d* ...)]
           [,else `(quote ,(strip else))]))]
      [(,fi ,[pred] ,[con] ,[alt]) (guard (core-form? fi if)) `(if ,pred ,con ,alt)]
      [((,def ,id ,[expr]) ,e* ...) (guard (core-form? def define))
         (let ((res (map (lambda (e) (expand e (ext-env id #f env))) e*)))                          
           `((define ,id ,expr) ,res ...))]
      [((,def-syn ,id ,[expr]) ,e* ...) (guard (core-form? def-syn define-syntax))
        (let ((trans (build-transformer expr)))
          (if (procedure? trans) 
              (let ((res (map (lambda (e) (expand e (ext-env def-syn trans env))) e*)))
                `((void) ,e* ...))
              (error 'expand "Invalid syntax: ~s" expr)))]
      [(,let-syn ((,id ,rhs) ...) ,e* ...)
        (guard (core-form? let-syn let-syntax) (and (set? (map expose `(,id ...)))))
        (let* ([var* (expose `(,id ...))]
               [s* (map (lambda (_) (lbl)) var*)])
          (expand 
            (do-subst e* var* s*) 
            (ext-env s* (map (lambda (rhs) (eval-trans (expand rhs env))) `(,rhs ...)) env)))]
;;   (rec-syn i e1 e2)  -> (expand (subst e2 i s) (ext-env s t r))
;;                           Where t = eval (expand (subst e1 i s) r) and s is fresh
      [(,core ,stx* ...) (guard (core-form? core)) 
       (error 'expand "Bad syntax: ~s" `(,core ,stx* ...))]
      [(,macro ,stx* ...)
       (guard (identifier? macro) (procedure? (apply-env env (resolve macro))))
       (expand (push-marks ((apply-env env (resolve macro)) (push-marks `(,macro ,stx* ...) anti-mark)) (mk)) env)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,id (guard (identifier? id))
           (let ([r (apply-env env (resolve id))])
             (if (procedure? r) 
                 (expand (push-marks (r (push-marks id anti-mark)) (mk)) env)
                 r))]
      [,wrapped (guard (wrap? wrapped)) (expand wrapped env)]
      [,const const])))

;;           [,k (guard (constant? k)) `(quote ,k)]
;;           [,id (guard (symbol? id)) ((apply-env id env) env id)]
;;           [(if ,[(Expr env) -> e1] ,[(Expr env) -> e2] ,[(Expr env) -> e3])
;;            `(if ,e1 ,e2 ,e3)]
;;           [(begin ,[(Expr env) -> e1] ,[(Expr env) -> e*] ...) `(,e1 ,e* ...)]
;;           [(lambda ,args ,e ...) 
;;            `(lambda ,args ,((Expr (remove-vars args env)) e) ...)]
;;           [(letrec ([,v* ,e*] ...) ,e ...) 
;;            (let ((newRHS (map (Expr (remove-vars v* env)) e*))
;;                  (newBody (map (Expr (remove-vars v* env)) e)))
;;              `(letrec [(,v* ,newRHS) ...] ,newBody ...))]
;;           [(let ([,v* ,[(Expr env) -> e*]] ...) ,e ...)
;;            (let ((newBody (map (Expr (remove-vars v* env)) e)))
;;              `(let ([,v* ,e*] ...) ,newBody ...))]
;;           [(letrec-syntax [(,v* ,(build-transformer -> r*)) ...] ,e ...) 
;;            (map (Expr (ext-env* `((,v* ,r*) ...) env)) e)]
;;           [(,id . ,stuff) (guard (symbol? id) (assq id env)) 
;;            ((Expr env) (apply-env expr env))]
;;           [(,[(Expr env) -> e1] ,[(Expr env) -> e*] ...) `(,e1 ,e* ...)]
;;           [,x (errorf who "invalid Expr ~s" x)])))


(trace-define syn-parse
  (lambda (expr pat keywords kt kf)
    (trace-define cull-list
      (lambda (e)
        (cond
          [(null? e) '()]
          [(pair? e) (cons (car e) (cull-list (cdr e)))]
          [else (kf expr)])))
    ;; Each success continuation takes a list of new bindings.
    (define dispatch
      (lambda (e p k)
        (match p
          [#t (k '())]
          [,x (guard (memq x keywords) (eq? x e)) (k '())]
          [,x (guard (symbol? x))
           (if (not (null? e)) (k `((,x . ,e))) (kf expr))]
          [(,pcar . ,pcdr)
           (cond
             [(and (pair? pcdr) (eq? (car pcdr) '...) (null? (cdr pcdr)))
              (k `((,pcar . ,(cull-list e))))]
             [(pair? e)
              (dispatch (car e) pcar 
                        (lambda (binds)
                          (dispatch (cdr e) pcdr 
                                    (lambda (binds^)
                                      (k (append binds binds^))))))]
             [else (kf expr)])]
          [() (if (null? e) (k '()) (kf expr))]
          [,x (equal? p e)])))
    (dispatch expr pat kt)))

;; Make hygiene do what it should
;; Look for Kent's notes on Advanced Compilers
(trace-define splice-syntax
  (lambda (keywords rhs)
    (trace-lambda 'splicer (bindings)
      (let* ((identifiers (map car bindings))
             (binded (map cdr bindings))
             (clean-names (map (lambda (x) (if (symbol? x) (unique-name x) x)) binded))
             (bindings (zip identifiers clean-names)))
        (trace-define rewriter
          (lambda (e)
            (match e
              [,x (guard (memq x identifiers)) (cdr (assq x bindings))]
              [(,ecar . ,ecdr)
                (cond
                   [(and (pair? ecdr) (eq? (car ecdr) '...) (null? (cdr ecdr)))
                    (let ((f (and (memq ecar identifiers) (assq ecar bindings)))) 
                      (if (and f (list? (cdr f))) (cdr f)
                          (errorf 'expander "Invalid usage of ... in rhs: ~s" rhs)))]
                   [else (cons (rewriter ecar) (rewriter ecdr))])]
              [() '()]
              [,x x])))
        (printf "~s~n" bindings)
        `(let ,(filter (lambda (x) (symbol? (car x))) (zip clean-names binded)) ,(rewriter rhs))))))

(trace-define build-transformer
  (lambda (expr)
    (match (expose expr)
      [(syntax-case ,x ,literals) 
       (lambda (e) (errorf 'expander "Invald syntax: ~s" expr))]
      [(syntax-case ,x ,literals (,pat ,rhs) . ,rest)
       (let ((kf (build-transformer `(syntax-case ,literals . ,rest)))
             (splicer (splice-syntax literals rhs)))
         (lambda (e) (syn-parse e pat literals splicer kf)))])))
           

(trace-define eval-trans
  (lambda (expr)
    (match (expose expr)
      [(lambda ,args ,body) (build-transformer (expose body))])))

;; (syntax-rules ((_ e1 e2) (+ e1 e2)) ((_ e1 e2 e3) (+ e1 e2 e3)))

;; (define and-t (build-transformer 
;;  'and 
;;   '(syntax-rules () 
;;     ((_) #t) 
;;     ((_ e) e)
;;     ((_ e e1 ...) (if e (and e1 ...) #f)))))

;; (define expand-syntax (lambda () (void)))
;; (define expand-syntax
;;   (lambda (prog)
;;     (define (ext-env* zipped env)
;;       (cond
;;         [(null? zipped) env]
;;         [else
;;           (let ([lhs (caar zipped)] [rhs (cdar zipped)])
;;             (cons `(,lhs . ,(var-helper lhs rhs))
;;                   (ext-env* (cdr zipped) env)))]))
;;     (define (extend-env x a env)
;;       `((,x . ,(var-helper x a)) . ,env))
;;     (define (apply-env y env)
;;       (let ([lookup (assq y env)])
;;         (if lookup
;;           (cdr lookup)
;;           (errorf who "Unbound variable ~s" y))))
;;     (define remove-args
;;       (lambda (args env)
;;         (cond
;;           [(null? args) env]
;;           [else (remove-args (cdr args) (remove (assq (car args) env) env))])))
;;     (define (Expr env)
;;       (lambda (expr)
;;         (match expr
;;           [,k (guard (constant? k)) `(quote ,k)]
;;           [,id (guard (symbol? id)) ((apply-env id env) env id)]
;;           [(if ,[(Expr env) -> e1] ,[(Expr env) -> e2] ,[(Expr env) -> e3])
;;            `(if ,e1 ,e2 ,e3)]
;;           [(begin ,[(Expr env) -> e1] ,[(Expr env) -> e*] ...) `(,e1 ,e* ...)]
;;           [(lambda ,args ,e ...) 
;;            `(lambda ,args ,((Expr (remove-vars args env)) e) ...)]
;;           [(letrec ([,v* ,e*] ...) ,e ...) 
;;            (let ((newRHS (map (Expr (remove-vars v* env)) e*))
;;                  (newBody (map (Expr (remove-vars v* env)) e)))
;;              `(letrec [(,v* ,newRHS) ...] ,newBody ...))]
;;           [(let ([,v* ,[(Expr env) -> e*]] ...) ,e ...)
;;            (let ((newBody (map (Expr (remove-vars v* env)) e)))
;;              `(let ([,v* ,e*] ...) ,newBody ...))]
;;           [(letrec-syntax [(,v* ,(build-transformer -> r*)) ...] ,e ...) 
;;            (map (Expr (ext-env* `((,v* ,r*) ...) env)) e)]
;;           [(,id . ,stuff) (guard (symbol? id) (assq id env)) 
;;            ((Expr env) (apply-env expr env))]
;;           [(,[(Expr env) -> e1] ,[(Expr env) -> e*] ...) `(,e1 ,e* ...)]
;;           [,x (errorf who "invalid Expr ~s" x)])))
;;     (Expr prog)))

(print-gensym 'pretty/suffix)
)
