(library (compiler x86-generation)
         (export
          expose-frame-var
          expose-memory-operands
          expose-basic-blocks
          optimize-jumps
          eliminate-cform
          flatten-program
          collect-strings
          generate-x86-64)
         (import
          ;; Load Chez Scheme primitives:
          (chezscheme)
          ;; Load provided compiler framework:
          (framework wrappers)
          (framework match)
          (framework helpers)
          (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | expose-frame-var                                              |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (expose-frame-var expr)                                       |
;; +-------------------------------------------------------------------------+
;; | RETURNS | Returns the tree with all frame vars translated into addrs    |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This does a natural, structure-agnostic tree walk. Whenever it finds a  |
;; | frame variable, it calls the appropriate expansion function and simply  |
;; | returns the result.                                                     |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Tail)]*) Tail)                    |
;; | Tail    -> (Triv)                                                       |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Loc Triv)                                              |
;; |          | (set! Loc (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> Loc | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Tail)]*) Tail)                    |
;; | Tail    -> (Triv)                                                       |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Loc Triv)                                              |
;; |          | (set! Loc (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> Loc | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who (expose-frame-var expr)
  (define fp frame-pointer-register)
  (define (walk expr fv)
    (match expr
      [(set! ,v (+ ,v ,num)) (guard (eq? v fp))
        (values expr (+ fv num))]
      [(set! ,v (- ,v ,num)) (guard (eq? v fp))
        (values expr (- fv num))]
      [,a (guard (atom? expr)) 
        (if (frame-var? a)
          (let* ([n (frame-var->index expr)]
                 [offset (- (ash n word-shift) fv)])
            (values (make-disp-opnd frame-pointer-register offset) fv))
          (values expr fv))]
      [(,a . ,d)
        (let*-values ([(a-expr new-fv) (walk a fv)]
                      [(d-expr ret-fv) (walk d new-fv)])
          (values (cons a-expr d-expr) ret-fv))]))
  (define (walker expr) ;; Texas Ranger
    (let-values ([(expr hukarz) (walk expr 0)])
      expr))
  (define (Program expr)
    (match expr
      [(letrec ([,lbl (lambda () ,[walker -> t*])] ...) ,[walker -> t])
       `(letrec ([,lbl (lambda () ,t*)] ...) ,t)]))
  (Program expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | expose-memory-operands                                        |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (expose-memory-operands expr)                                 |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with memory operands exposed.                  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass inserts the make-opnd form for mset! and mref forms.          |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Another day, another tree walk.                                         |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Tail)]*) Tail)                    |
;; | Tail    -> (Triv)                                                       |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Loc Triv)                                              |
;; |          | (set! Loc (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> Loc | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Tail)]*) Tail)                    |
;; | Tail    -> (Triv)                                                       |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Loc Triv)                                              |
;; |          | (set! Loc (binop Triv Triv))                                 |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> Loc | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who (expose-memory-operands expr)
  (define (walk expr)
    (match expr
      [,x (guard (atom? x)) x]
      [(mset! ,base ,offset ,val)
        (if (register? offset)
            `(set! ,(make-index-opnd base offset) ,val)
            `(set! ,(make-disp-opnd base offset) ,val))]
      [(mref ,base ,offset)
        (if (register? offset)
            (make-index-opnd base offset)
            (make-disp-opnd base offset))]
      [(,a . ,d)
        (cons (walk a) (walk d))]))
  (define (Program expr)
    (match expr
      [(letrec ([,lbl (lambda () ,[walk -> t*])] ...) ,[walk -> t])
       `(letrec ([,lbl (lambda () ,t*)] ...) ,t)]))
  (Program expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | expose-basic-blocks                                           |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (expose-basic-blocks scheme-expr)                             |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with basic blocsk exposed - much flattened.    |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | The goal of this pass is to reduce the language down for                |
;; | flatten-program. We have the limited form of if expressions in tail     |
;; | context, where they amount to two-way conditional jumps. Along the way, |
;; | it must introduce new labels to handle the conditional control ﬂow and  |
;; | bind these labels in the top-level letrec to procedures that represent  |
;; | the codeto be executed at the target of each jump.                      |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([label (lambda () Tail)]*) Tail)                   |
;; |  Tail    -> (Triv)                                                      |
;; |           | (ccall reg label Triv*)                                     |
;; |           | (if Pred Tail Tail)                                         |
;; |           | (begin Effect* Tail)                                        |
;; |  Pred    -> (true)                                                      |
;; |           | (false)                                                     |
;; |           | (relop Triv Triv)                                           |
;; |           | (if Pred Pred Pred)                                         |
;; |           | (begin Effect* Pred)                                        |
;; |  Effect  -> (nop)                                                       |
;; |           | (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |           | (return-point lbl Tail)                                     |
;; |           | (if Pred Effect Effect)                                     |
;; |           | (begin Effect* Effect)                                      |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label                                           |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([label (lambda () Tail)]*) Tail)                   |
;; |  Tail    -> (Triv)                                                      |
;; |           | (ccall reg label Triv*)                                     |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (begin Effect* Tail)                                        |
;; |  Effect  -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label                                           |
;; +-------------------------------------------------------------------------+

(define-who expose-basic-blocks
  (define (Tail body)
    (match body
      [(ccall ,reg ,lbl ,triv* ...) 
       (values `(ccall ,reg ,lbl ,triv* ...) '())] 
      [(,x) (guard (or (label? x) (register? x) (disp-opnd? x)))
       (values `(,x) '())]
      [(if ,pred ,con ,alt)
       (let*-values 
    	   ([(con-lbl) (unique-label 'c)]
    	    [(alt-lbl) (unique-label 'a)]
    	    [(cb cbind) (Tail con)]
    	    [(ab abind) (Tail alt)]
    	    [(pb pbind) (Pred pred con-lbl alt-lbl)])
      	 (values pb `(,pbind ... 
			     (,con-lbl (lambda () ,cb)) ,cbind ...
			     (,alt-lbl (lambda () ,ab)) ,abind ...)))]
      [(begin ,e* ... ,t)
       (let*-values ([(tbody tbindings) (Tail t)]
		     [(ebody ebindings) (Effect* e* `(,tbody) '())])
      	 (values ebody `(,ebindings ... ,tbindings ...)))]))
  (define (Pred pred con-lbl alt-lbl)
    (match pred
      [(true) (values `(,con-lbl) '())]
      [(false) (values `(,alt-lbl) '())]
      [(begin ,e* ... ,p) 
       (let-values ([(plbl pbdy) (Pred p con-lbl alt-lbl)])
         (Effect* `(,e* ...) `(,plbl) pbdy))]
      [(if ,p ,c ,a) 
       (let*-values ([(cb cbind) (Pred c con-lbl alt-lbl)]
		     [(ab abind) (Pred a con-lbl alt-lbl)]
		     [(new-con-lbl) (unique-label 'c)]
		     [(new-alt-lbl) (unique-label 'a)]
		     [(pb pbind) 
		      (Pred p new-con-lbl new-alt-lbl)])
	       (values pb `(,pbind ...
		  	     (,new-con-lbl (lambda () ,(make-begin (list cb))))
			         ,cbind ...
			       (,new-alt-lbl (lambda () ,(make-begin (list ab))))
			       ,abind ...)))]
      [(,relop ,t1 ,t2) 
       (values `(if (,relop ,t1 ,t2) (,con-lbl) (,alt-lbl)) '())]))
  (define (Effect* expr tail bindings)
    (match expr
      [() (values (make-begin tail) bindings)]
      [(,e* ... (nop)) 
       (Effect* `(,e* ...) tail bindings)]
      [(,e* ... (set! ,a ,b)) 
       (Effect* `(,e* ...) `((set! ,a ,b) ,tail ...) bindings)]
      [(,e* ... (return-point ,lbl ,t))
         (let-values 
           ([(tb tbind) (Tail t)])
           (Effect* `(,e* ...) `(,tb) 
                    `(,tbind ... ,bindings ... 
                          (,lbl (lambda () ,(make-begin tail))))))]
      [(,e* ... (if ,p ,con ,alt)) 
       (let*-values 
      	 ([(con-lbl) (unique-label 'c)]
    	    [(alt-lbl) (unique-label 'a)]
    	    [(jmp-lbl) (unique-label 'j)]
      	  [(cb cbind) (Effect* (list con) (list (list jmp-lbl)) '())]
      	  [(ab abind) (Effect* (list alt) (list (list jmp-lbl)) '())]
      	  [(pb pbind) (Pred p con-lbl alt-lbl)])
             (Effect* `(,e* ...) `(,pb) `(,pbind ... 
    		  	   (,con-lbl (lambda () ,(make-begin (list cb))))
                  			     ,cbind ...
                             (,alt-lbl (lambda () ,(make-begin (list ab))))
                             ,abind ...
                             (,jmp-lbl (lambda () ,(make-begin tail)))
                             ,bindings ...)))]
      [(,e* ... (begin ,e^ ... ,e))  
         (Effect* `(,e* ... ,e^ ... ,e) tail bindings)]))
  (lambda (expr)
    (match expr
      [(letrec 
           ([,labels (lambda () ,[Tail -> bodies* b**])] ... ) 
         ,[Tail -> body b*])
       `(letrec 
            ([,labels (lambda () ,bodies*)] ... ,b** ... ... ,b* ...) ,body)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | optimize-jumps                                                |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (optimize-jumps expr)                                         |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression, except that any label whose entire body is a  |
;; |         | call to another label is instead inlined.                     |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass ensures that any jump who simply jumps to another label is    |
;; | inlined directly.                                                       |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Tree Walks! One to find the labels, the other to do the swap.           |
;; +-------------------------------------------------------------------------+
;; | INPUT/OUTPUT GRAMMAR                                                    |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([label (lambda () Tail)]*) Tail)                   |
;; |  Tail    -> (Triv)                                                      |
;; |           | (ccall reg label Triv*)                                      |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (begin Effect* Tail)                                        |
;; |  Effect  -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label                                           |
;; +-------------------------------------------------------------------------+
(define-who (optimize-jumps expr)
  (define dumb-jumps '())
  (define (check-label expr)
    (match expr
      [(,l1 (lambda () (,l2))) (guard (and (label? l1) (label? l2)))
         (set! dumb-jumps (cons `(,l1 . ,l2) dumb-jumps))]
      [(,l1 (lambda () ,x)) `(l1 (lambda () ,x))]
      [() '()]))
  (define (lookup var S visited)
    (let ([ a (assq var S)])
      (cond
        [(and a (assq (cdr a) visited)) (cdr a)]
        [a (lookup (cdr a) S (cons a visited))]
        [else var])))
  (define (compress working lbls)
    (cond
      [(null? working) '()]
      [else
        (let* ([a (caar working)] [d (lookup a lbls '())])
              (if (eq? a d) 
               (compress (cdr working) lbls)
                (cons `(,a . ,d) (compress (cdr working) lbls))))]))
  (define (inline-labels-walk expr jumps)
    (match expr
      [() '()]
      [(,x) (guard (label? x))
        (let ([lbl (assq x jumps)]) (if lbl `(,(cdr lbl)) `(,x)))]
      [,x (guard (atom? x)) x]
      [(,a . ,d) 
        `(,(inline-labels-walk a jumps) . ,(inline-labels-walk d jumps))]))
  (define (inline-label expr jumps)
    (match expr
      [(,lbl (lambda () ,tail))
        `(,lbl (lambda () ,(inline-labels-walk tail jumps)))]
      [() '()]))
  (define (prune-labels ls* jumps)
    (cond
      [(null? ls*) '()]
      [(assq (caar ls*) jumps) (prune-labels (cdr ls*) jumps)]
      [else (cons (car ls*) (prune-labels (cdr ls*) jumps))]))
  (match expr
    [(letrec ,lambda* ,t)
      (map check-label lambda*)
      (let ([jumps (compress dumb-jumps dumb-jumps)])
      `(letrec 
         ,(map (lambda (e) (inline-label e jumps)) (prune-labels lambda* jumps)) 
         ,(inline-labels-walk t jumps)))]))

;; +-------------------------------------------------------------------------+
;; | PASS    | eliminate-ccall                                               |
;; | AUTHOR  | Cameron Swords and Andre Kuhlenschmidt                        |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (eliminate-ccall expr)                                        |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The program with ccalls 'unraveled'                           |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass removes any ccall forms and sets up the actual calls instead. |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Tree Walk! It finds the ccall forms and fixes them.                     |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([label (lambda () Tail)]*) Tail)                   |
;; |  Tail    -> (Triv)                                                      |
;; |           | (ccall reg label Triv*)                                     |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (begin Effect* Tail)                                        |
;; |  Effect  -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label | String                                  |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([label (lambda () Tail)]*) Tail)                   |
;; |  Tail    -> (Triv)                                                      |
;; |           | (ccall reg label Triv*)                                     |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (begin Effect* Tail)                                        |
;; |  Effect  -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |           | (push <Triv | Loc>)                                         |
;; |           | (pop Loc)                                                   |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label | String                                  |
;; +-------------------------------------------------------------------------+

(define-who (eliminate-cform expr)
  (define fpr frame-pointer-register)
  (define rar return-address-register)
  (define (walk expr)
    (match expr
      [,x (guard (atom? x)) x]
      [(ccall ,reg ,lbl ,triv* ...)
       (if (eqv? reg return-value-register)
           (make-begin `((call ,lbl) (,rar)))
           (make-begin `((call ,lbl) (set! ,reg ,return-value-register) (,rar))))]
      [(,a . ,d)
        (cons (walk a) (walk d))]))
  (define (Program expr)
    (match expr
      [(letrec ([,lbl (lambda () ,[walk -> t*])] ...) ,[walk -> t])
       `(letrec ([,lbl (lambda () ,t*)] ...) ,t)]))
  (Program expr))

;; +-------------------------------------------------------------------------+
;; | PASS    | flatten-program                                               |
;; | AUTHOR  | Cameron Swords and Andre Kuhlenschmidt                        |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (flatten-program expr)                                        |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The program in a form much closer to assembly                 |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass flattens out the now slightly nested structure of our source  |
;; | language into one that more closely resembles assembly language, no     |
;; | letrec, no begin forms, calls turned into explicit jumps, and the names |
;; | of procedures turned into label forms. It produces a single code form   |
;; | containing a sequence of labels, effect expressions, and jumps, with the|
;; | code for the body of the letrec appearing first followed by the body of |
;; | each lambda expression in turn, prefixed by its label.                  |
;; |                                                                         |
;; | Also, it does something clever with labels and ifs...                   |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([label (lambda () Tail)]*) Tail)                   |
;; |  Tail    -> (Triv)                                                      |
;; |           | (ccall reg label Triv*)                                     |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (begin Effect* Tail)                                        |
;; |  Effect  -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |           | (push <Triv | Loc>)                                         |
;; |           | (pop Loc)                                                   |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label | String                                  |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; |  Program -> (code (Stmt)* (Jump) label (Stmt*) (Jump) ... )             |
;; |  Stmt    -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (if (not (relop Triv Triv)) (label) (label))                |
;; |           | (push <Triv | Loc>)                                         |
;; |           | (pop Loc)                                                   |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label | String                                  |
;; +-------------------------------------------------------------------------+

(define-who flatten-program
  (define (Effect expr) expr)
  (define (Tail* expr* lbl*)
    (cond
      [(null? (cdr lbl*)) (cons* (car lbl*) (Tail (car expr*) #f))]
      [else 
        (append (list (car lbl*)) 
                (Tail (car expr*) (cadr expr*)) 
                (Tail* (cdr expr*) (cdr lbl*)))]))
  (define (Tail expr next-label)
    (match expr
      [(begin ,[Effect -> e*] ... ,t) `(,e* ... ,(Tail t next-label) ...)]
      [(,if (,r ,t1 ,t2) (,l1) (,l2))
       (cond
         [(equal? l1 next-label) `((if (not (,r ,t1 ,t2)) (jump ,l2)))]
         [(equal? l2 next-label) `((if (,r ,t1 ,t2) (jump ,l1)))]
         [else `((if (,r ,t1 ,t2) (jump ,l1)) (jump ,l2))])]
      [(,call) (guard (or (disp-opnd? call) (register? call) (label? call)))
       (if (not (equal? call next-label)) `((jump ,call)) '())] 
      [(pop ,x) `((pop ,x))]
      [(ccall . ,args) `((ccall . ,args))]))
  (define (interweave ls1 ls2)
    (fold-right (lambda (c1 c2 acc) (append `(,c1 ,@c2) acc)) '() ls1 ls2))
  (lambda (expr)
    (match expr
      [(letrec () ,t) `(code ,(Tail t #f) ...)]
      [(letrec ([ ,lbl* (lambda () ,t*)] ...) ,t)
         (let ((lbl-cdr (cdr lbl*)))
           `(code ,(Tail t (car lbl*)) ... ,@(Tail* `(,t* ... ) `(,lbl* ...))))])))

;; +-------------------------------------------------------------------------+
;; | PASS    | collect-strings                                               |
;; | AUTHOR  | Cameron Swords and Andre Kuhlenschmidt                        |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (collect-strings expr)                                        |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The program with the strings in their own labels              |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass walks the entire program, finding any raw strings that occur  |
;; | and moving them to their own labels, replacing the calls with the label |
;; | references.                                                             |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; |  Program -> (code (Stmt)* (Jump) label (Stmt*) (Jump) ... )             |
;; |  Stmt    -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (if (not (relop Triv Triv)) (label) (label))                |
;; |           | (push <Triv | Loc>)                                         |
;; |           | (pop Loc)                                                   |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label | String                                  |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; |  Program -> ((code (Stmt)* (Jump) label (Stmt*) (Jump) ... )            |
;; |              section (Stmt*) section (Stmt*) ...)                       |
;; |  Stmt    -> (set! Loc Triv)                                             |
;; |           | (set! Loc (binop Triv Triv))                                |
;; |           | (if (relop Triv Triv) (label) (label))                      |
;; |           | (if (not (relop Triv Triv)) (label) (label))                |
;; |           | (push <Triv | Loc>)                                         |
;; |           | (push-prim-lbl Triv)                                        |
;; |           | (pop Loc)                                                   |
;; |  Loc     -> reg | disp-opnd                                             |
;; |  Triv    -> Loc | int | label | String                                  |
;; +-------------------------------------------------------------------------+

(define-who (collect-strings expr)
  (define new-sections '())
  (define (Stmt expr)
    (match expr
      [(set! ,loc ,str) (guard (string? str) (not (label? str)) (not (register? str))) 
         (let ((sec (symbol-append (string->symbol ".") (unique-label 'string)))) 
           (set! new-sections (cons* sec `(string ,str) '(text) new-sections))
           `(set! ,loc ,sec))]
      [,x x]))
  (match expr
    [(code ,[Stmt -> s] ...) `((code ,s ...) ,new-sections ...)]))

;; +-------------------------------------------------------------------------+
;; | PASS    | generate-x86-64                                               |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (generate-x86-64 expr)                                        |
;; +-------------------------------------------------------------------------+
;; | RETURNS | Returns the complete program translated into x86-64 assembly. |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass uses a combinator-parser approach to parsing in order to      |
;; | produce valid x86-64 assembly that properly reproduces the operations   |
;; | in the source program.                                                  |
;; +-------------------------------------------------------------------------+

;; using the cmpq and conditional jump instructions je, jne, jl, jle, jg, and jge.

(define-who generate-x86-64
  (define prim->inst
    (lambda (op)
      (case op
        [(+) 'addq]
        [(-) 'subq]
        [(*) 'imulq]
        [(logand) 'andq]
        [(logor) 'orq]
        [(sra) 'sarq]
        [else (errorf who "unexpected binop ~s" op)])))
  (define rel->inst
     (lambda (op n)
       (if (not n)
         (case op
            [(<) 'jl] [(<=) 'jle] [(=) 'je] [(>=) 'jge] [(>) 'jg]
            [else (errorf who "What op did you mean by ~s" op)])
          (case op
            [(<) 'jge] [(<=) 'jg] [(=) 'jne] [(>=) 'jl] [(>) 'jle]
            [else (errorf who "What op did you mean by ~s" op)]))))
  (define Statement
    (lambda (st)
      (match st
        [,lbl (guard (section? lbl)) (emit-section lbl)]
        [,lbl (guard (label? lbl)) (emit-label lbl)]
        [(push ,val) (emit 'pushq val)]
        [(pop ,val) (emit 'popq val)]
        [(call ,callee) (emit-call callee)]
        [(string ,s) (emit-string s)]
        [(text) (emit-text)]
        [(set! ,dest (,prim ,dest ,source))
         (emit (prim->inst prim) source dest)]
        [(set! ,dest ,source) (guard (section? source)) (emit 'movq source dest)]
        [(set! ,dest ,source) (guard (label? dest)) (emit 'leaq source dest)]
        [(set! ,dest ,source) (guard (label? source)) (emit 'leaq source dest)]
        [(set! ,dest ,source) (emit 'movq source dest)]
        [(if (not (,op ,t1 ,t2)) (jump ,dest))
            (begin
              (emit 'cmpq t2 t1)
              (emit-jump (rel->inst op #t) dest))]
        [(if (,op ,t1 ,t2) (jump ,dest))
            (begin
              (emit 'cmpq t2 t1)
              (emit-jump (rel->inst op #f) dest))]
        [(jump ,dest) (emit-jump 'jmp dest)]
        [,stmt (errorf who "unexpected statement ~s" stmt)])))
  (lambda (expr)
    (match expr
      [((code ,stmt* ...) ,sec* ...) 
       (emit-program (entry (for-each Statement stmt*)) (for-each Statement sec*))]
      [,x (errorf who "unexpected program ~s" x)])))

) ;; END LIBRARY
