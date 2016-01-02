(library (compiler enforce-conventions)
         (export
          remove-complex-opera*
          flatten-set!
          impose-calling-conventions)
         (import
          ;; Load Chez Scheme primitives:
          (chezscheme)
          ;; Load provided compiler framework:
          (framework wrappers)
          (framework match)
          (framework helpers)
          (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | remove-complex-opera*                                         |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (remove-complex-opera* expr)                                  |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with all nest primitive calls made trivial.    |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass removes nested primitive calls from within procedure calls    |
;; | and other primitive calls, making the argument values 'trivial'. Some   |
;; | programs produced by this pass are in  the language described by the    |
;; | grammar below.                                                          |
;; |                                                                         |
;; | In order to carry this out, each nontrivial Value must must be assigned |
;; | outside of the call to new, unique variable.                            |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Grammar walk with expression rewriting occurs to remove any complex     |
;; | operation and produce only non-complex operations.                      |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (alloc Value)                                                |
;; |          | (mref Value Value)                                           |
;; |          | (binop Value Value)                                          |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Value Value)                                          |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (ccall reg label Value*)                                     |
;; |          | (set! uvar Value)                                            |
;; |          | (mset! Value Value Value)                                    |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Value   -> Triv                                                         |
;; |          | (alloc Value)                                                |
;; |          | (mref Value Value)                                           |
;; |          | (binop Value Value)                                          |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (alloc Triv)                                                 |
;; |          | (mref Triv Triv)                                             |
;; |          | (binop Triv Triv)                                            |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (ccall reg label Value*)                                     |
;; |          | (set! uvar Value)                                            |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Value   -> Triv                                                         |
;; |          | (alloc Triv)                                                 |
;; |          | (mref Triv Triv)                                             |
;; |          | (binop Triv Triv)                                            |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+

(define-who remove-complex-opera*
  (define (binop? x)
    (memq x '(* + - logand logor sra mref)))
  (define (Body bd)
    (define new-local* '())
    (define (new-t)
      (let ([t (unique-name 't)])
        (set! new-local* (cons t new-local*))
        t))
    (define (trivialize-call expr*)
      (let-values ([(call set*) (break-down-expr* expr*)])
        (make-begin `(,@set* ,call))))
    (define (break-down-expr* expr*)
      (match expr*
        [() (values '() '())]
        [(,s . ,[rest* set*]) 
         (guard (simple? s)) 
         (values `(,s ,rest* ...) set*)]
        [(,[Value -> expr] . ,[rest* set*])
         (let ([t (new-t)]) 
           (values `(,t ,rest* ...) `((set! ,t ,expr) ,set* ...)))]
        [,expr* (error who "invalid Expr ~s" expr*)]))
    (define (simple? x)
      (or (eq? x 'alloc) (eq? x 'mset!) (eq? x 'ccall) (cfunc? x)
          (uvar? x) (label? x) (and (integer? x) (exact? x))
          (binop? x) (register? x) (memq x '(= < <= > >=))))
    (define (triv? x) (or (uvar? x) (int64? x) (label? x)))
    (define (Value val)
      (match val
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[val]) (make-begin `(,ef* ... ,val))]
        [(alloc ,n)
          (trivialize-call `(alloc ,n))]
        [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
        [(,binop ,x ,y)
         (guard (binop? binop))
         (trivialize-call `(,binop ,x ,y))]
        [,tr (guard (triv? tr)) tr]
        [,val (error who "invalid Value ~s" val)]))
    (define (Effect ef)
      (match ef
        [(nop) '(nop)]
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
        [(set! ,var ,[Value -> val]) `(set! ,var ,val)]
        [(mset! ,v1 ,v2 ,v3)
         (trivialize-call `(mset! ,v1 ,v2 ,v3))]
        [(ccall ,reg ,lbl ,arg ...) 
         (trivialize-call `(ccall ,reg ,lbl ,arg ...))] 
        [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
        [,ef (error who "invalid Effect ~s" ef)]))
    (define (Pred pr)
      (match pr
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
        [(,relop ,x ,y)
         (guard (memq relop '(< <= = >= >)))
         (trivialize-call `(,relop ,x ,y))]
        [,pr (error who "invalid Pred ~s" pr)]))
    (define (Tail tail)
      (match tail
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
        [(alloc ,n)
          (trivialize-call `(alloc ,n))]
        [(,binop ,x ,y)
         (guard (binop? binop))
         (trivialize-call `(,binop ,x ,y))]
        [(,rator ,rand* ...) (trivialize-call `(,rator ,rand* ...))]
        [,tr (guard (triv? tr)) tr]
        [,tail (error who "invalid Tail ~s" tail)]))
    (match bd
      [(locals (,local* ...) ,[Tail -> tail])
       `(locals (,local* ... ,new-local* ...) ,tail)]
      [,bd (error who "invalid Body ~s" bd)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...) 
         ,[Body -> bd])
       `(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | flatten-set!                                                  |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (flatten-set! expr)                                           |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with set!s flattened out.                      |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass fuses set!s inside of if and begin statements. It does this   |
;; | until the set!s are all inside begins and ifs - none are outside of a   |
;; | call. This lets us remove the Value non-terminal from our language.     |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | TODO                                                                    |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (alloc Triv)                                                 |
;; |          | (binop Triv Triv)                                            |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (ccall reg label Value*)                                     |
;; |          | (set! uvar Value)                                            |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (ccall reg label Triv*)                                      |
;; | Value   -> Triv                                                         |
;; |          | (alloc Triv)                                                 |
;; |          | (binop Triv Triv)                                            |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (alloc Triv)                                                 |
;; |          | (binop Triv Triv)                                            |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! uvar Triv)                                             |
;; |          | (set! uvar (binop Triv Triv))                                |
;; |          | (set! uvar (Triv Triv*))                                     |
;; |          | (set! uvar (alloc Triv))                                     |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+

(define-who flatten-set! ;; no changes made ... hmmm ...
  (define (triv? x)
    (or (uvar? x) (int64? x) (label? x)))
  (define (Body expr)
    (match expr
      [(locals (,u* ...) ,[walk -> t])
        `(locals (,u* ...) ,t)]))
  (define (walk expr)
   (match expr
      [,t (guard (atom? t)) t]
      [(set! ,v (begin ,e* ... ,e)) 
        (make-begin `(,(walk e*) ... ,(walk `(set! ,v ,e))))]
      [(set! ,v (if ,e1 ,e2 ,e3))
        `(if ,(walk e1) ,(walk `(set! ,v ,e2)) ,(walk `(set! ,v ,e3)))]
      [(begin ,e* ... ,e) (make-begin `(,(walk e*) ... ,(walk e)))]
      [(,a . ,d) `(,(walk a) . ,(walk d))]))
  (lambda (expr)
    (match expr
      [(letrec ([,l* (lambda (,v ...) ,[Body -> b*])] ...) ,[Body -> b])
        `(letrec ([,l* (lambda (,v ...) ,b*)] ...) ,b)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | impose-calling-conventions                                    |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (impose-calling-conventions expr)                             |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with calling conventions imposed on calls.     |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Each call must deal with saving in-scope variables to ensure that, when |
;; | the function returns, each variable is still available. This pass does  |
;; | this by finding call positions and doing exactly that.                  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | This pass makes three transformations to achieve its effect:            |
;; |   1) It converts the formal parameters of each lambda expression into   |
;; |      locals and initializes these locals from the appropriate registers |
;; |      and frame locations.                                               |
;; |   2) It assigns the appropriate registers and frame locations to the    |
;; |      values of the actual parameters in each call. It replaces the      |
;; |      arguments in the syntax for each call with a set of locations      |
;; |      assumed to be live at the call.                                    |
;; |   3) This pass converts each Triv or primitive call tail into an        |
;; |      explicit assignment to the return-value register and a call to the |
;; |      return point. The return-value register is presumed live when the  |
;; |      call is made, so it is listed as live at the point of call. The    |
;; |      return-address register should not be included, since the last     |
;; |      reference to it is in the call itself.                             |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (alloc Triv)                                                 |
;; |          | (binop Triv Triv)                                            |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! uvar Triv)                                             |
;; |          | (set! uvar (binop Triv Triv))                                |
;; |          | (set! uvar (Triv Triv*))                                     |
;; |          | (set! uvar (alloc Triv))                                     |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (Triv Triv*)                                                 |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) (new-frames (Frame*) Tail))                  |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; |          | (ccall reg label Triv*)                                      |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! uvar Triv)                                             |
;; |          | (set! uvar (binop Triv Triv))                                |
;; |          | (set! uvar (alloc Triv))                                     |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> uvar | int | label                                           |
;; | Frame   -> (uvar*)                                                      |
;; +-------------------------------------------------------------------------+

(define-who impose-calling-conventions ;; added alloc to tail position, alloc
  (define (triv? x)                          ;; register as a live
    (or (uvar? x) (int64? x) (label? x)))
  (define (binop? x)
    (memq x '(* + - logand logor sra mref)))
  (define (ret-lbl) (unique-label 'ret))
  (define ap allocation-pointer-register)
  (define fp frame-pointer-register)
  (define ra return-address-register)
  (define rv return-value-register)
  (define preg-len (length parameter-registers))
  (define (Body args b)
    (define (locate-cargs args)
      (zip args '(rdi rsi rdx rcx r8 r9)))
    (define (locate-nv-args args) ;; empty list may be wrong
      (locate-nv-args-helper args parameter-registers '()))
    (define (locate-stack-args args)
      (locate-nv-args-helper args '() '()))
    (define (locate-nv-args-helper args pregs new-frame)
      (cond
        [(null? args)
          (if (not (null? new-frame))
            (set! new-frame-vars (cons (reverse new-frame) new-frame-vars)))
          '()]
        [(null? pregs)
          (let ([nfv (unique-name 'nfv)]) 
          (cons `(,(car args) . ,nfv)
            (locate-nv-args-helper (cdr args) pregs (cons nfv new-frame))))]
        [else (cons `(,(car args) . ,(car pregs))
            (locate-nv-args-helper (cdr args) (cdr pregs) new-frame))]))
    (define (locate-args args)
      (locate-args-helper args parameter-registers 0))
    (define (locate-args-helper args pregs fv)
      (cond
        [(null? args) '()]
        [(null? pregs) 
          (cons `(,(car args) . ,(index->frame-var fv))
            (locate-args-helper (cdr args) '() (add1 fv)))]
        [else (cons `(,(car args) . ,(car pregs))
            (locate-args-helper (cdr args) (cdr pregs) fv))]))
    (define rp (unique-name 'rp))
    (define new-frame-vars '())
    (define (Tail expr)
      (match expr
        [,t (guard (triv? t)) 
          (make-begin `((set! ,rv ,t) (,rp ,fp ,rv ,ap)))]
        [(alloc ,size) 
          (make-begin `((set! ,rv (alloc ,size)) (,rp ,fp ,rv ,ap)))]
        [(,op ,x ,y) (guard (binop? op)) 
          (make-begin `((set! ,rv (,op ,x ,y)) (,rp ,fp ,rv ,ap)))]
        [(,t ,l* ...) (guard (triv? t))
          (let* ([arg-locs (locate-args l*)]
                 [new-args (map cdr arg-locs)]
                 [cmfv (max 0 (- (length new-args) preg-len))]
                 [set^ (map (lambda (e) `(set! ,(cdr e) ,(car e))) arg-locs)]
                 [sets (reverse set^)])
            (make-begin 
              `(,sets ... 
                (set! ,(index->frame-var cmfv) ,$contmark) 
                (set! ,ra ,rp) 
                (,t ,fp ,ra ,ap ,new-args ...))))]
        [(if ,[Pred -> p] ,[Tail -> t1] ,[Tail -> t2]) `(if ,p ,t1 ,t2)]
        [(begin ,[Effect -> e*] ... ,[Tail -> t]) (make-begin `(,e* ... ,t))]))
    (define (Pred p)
      (match p
        [(true) '(true)]
        [(false) '(false)]
        [(,op ,t1 ,t2) (guard (relop? op)) `(,op ,t1 ,t2)]
        [(if ,[Pred -> p1] ,[Pred -> p2] ,[Pred -> p3]) `(if ,p1 ,p2 ,p3)]
        [(begin ,[Effect -> e*] ... ,[Pred -> p]) (make-begin `(,e* ... ,p))]))
    (define (Effect e)
      (match e
        [(nop) '(nop)]
        [(set! ,v (,op ,t1 ,t2)) (guard (binop? op)) `(set! ,v (,op ,t1 ,t2))]
        [(set! ,v (alloc ,size)) `(set! ,v (alloc ,size))]
        [(set! ,v (,t ,t* ...))
          (make-begin
            `(,(Effect `(,t ,t* ...))
              (set! ,v ,rv)))]
        [(set! ,v ,t) `(set! ,v ,t)]
        [(mset! ,t1 ,t2 ,t3) `(mset! ,t1 ,t2 ,t3)]
        [(ccall ,reg ,lbl ,t* ...) 
          (let* ([rlbl (ret-lbl)]
                 [arg-locs (locate-cargs t*)]
                 [new-args (map cdr arg-locs)]
                 [allRegs (locate-stack-args registers)]
                 [set^ (map (lambda (e) `(set! ,(cdr e) ,(car e))) arg-locs)]
                 [set^ (map (lambda (e) `(set! ,(cdr e) ,(car e))) arg-locs)]
                 [sets (reverse set^)])
            `(return-point ,rlbl 
              (begin ,sets ... (set! ,ra ,rlbl) (ccall ,reg ,lbl))))]
        [(if ,[Pred -> p] ,[Effect -> e1] ,[Effect -> e2]) `(if ,p ,e1 ,e2)]
        [(begin ,[Effect -> e*] ... ,[Effect -> e]) (make-begin `(,e* ... ,e))]
        [(,t ,t* ...)
          (let* ([rlbl (ret-lbl)]
                 [arg-locs (locate-nv-args t*)]
                 [new-args (map cdr arg-locs)]
                 [set^ (map (lambda (e) `(set! ,(cdr e) ,(car e))) arg-locs)]
                 [sets (reverse set^)])
            `(return-point ,rlbl 
               (begin ,sets ... (set! ,ra ,rlbl) (,t ,fp ,ra ,ap ,new-args ...))))]))
    (match b
      [(locals (,u* ...) ,[Tail -> t])
        (let* ([arg-locs (locate-args args)]
               [sets (map (lambda (e) `(set! ,(car e) ,(cdr e))) arg-locs)])
          `(locals (,u* ... ,rp ,args ... ,new-frame-vars ... ...) 
            (new-frames ,new-frame-vars
            ,(make-begin `((set! ,rp ,ra) ,sets ... ,t)))))]))
;; (printf "~a ~a ~a ~a~n" ap fp ra rv)
  (lambda (expr)
    (match expr
      [(letrec ([,l (lambda ,arg* ,b*)] ...) ,b)
          (let ([bodies (map Body arg* b*)])
            `(letrec
              ([,l (lambda () ,bodies)] ...) ,(Body '() b)))])))

) ;; END LIBRARY
