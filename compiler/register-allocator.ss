(library (compiler register-allocator)
         (export
          select-instructions
          uncover-register-conflict
          assign-registers
          everybody-home?
          assign-frame
          finalize-frame-locations
          protect-ccall-lives
          discard-call-live
          finalize-locations)
         (import
          ;; Load Chez Scheme primitives:
          (chezscheme)
          ;; Load provided compiler framework:
          (framework wrappers)
          (framework match)
          (framework helpers)
          (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | HELPER  | do-uncover-conflict                                           |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (do-uncover-conflict expr uvar-list caller pred-test)         |
;; +-------------------------------------------------------------------------+
;; | RETURNS | A graph of conflicts between uvars in the list and anything   |
;; |         | matching the pred test.                                       |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Uncovers the conflicts for variables and registers where 'test' is the  |
;; | predicate to discern which type of variables are in conflict.           |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | For the input, assumed to be a body, I take a walk up the tree. I start |
;; | at the bottom of the body and walk toward the top, adding a new entry   |
;; | every time I come across a new register and returning my list at the    |
;; | top. I then stitch it into place.                                       |
;; |                                                                         |
;; | The actual walk probably doesn't have to be super-related to the input  |
;; | grammar, but it seems worthwhile for simplicity and future changes.     |
;; |                                                                         |
;; | So the way this works is that I use a 'stateful' graph variable and use |
;; | side effects to build up the graph as I walk the input. Then I return   |
;; | the graph.                                                              |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
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
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define (do-uncover-conflict expr uvar* who test)
  (define graph '())
  (define add-conflict
      (lambda (var1 var2)
        (cond
          [(eq? var1 var2)
            (let ([lookup (assq var1 graph)])
              (if (not lookup)
                (set! graph (cons `(,var1) graph))))]
          [else
            (let ([lookup (assq var1 graph)])
              (if lookup  
                (set-cdr! lookup (union (list var2) (cdr lookup)))
                (set! graph (cons `(,var1 ,var2) graph))))])))
  (define (add-conflicts lhs liveset)
    (cond
      [(if (or (uvar? lhs) (test lhs))
        (let ([liveset (filter (lambda (e) (or (uvar? e) (test e))) liveset)])
          (if (uvar? lhs) (map (lambda (e) (add-conflict lhs e)) liveset))
          (map (lambda (e) (if (uvar? e) (add-conflict e lhs))) liveset)))]
      [else (void)]))
  (define Triv (lambda (e) (if (or (uvar? e) (test e)) `(,e) '())))
  (define (Effect* expr liveset)
    (match expr
      [() liveset]
      [(,ef* ... ,ef) (Effect* ef* (Effect ef liveset))]))
  (define (Effect expr liveset)
    (match expr
      [(nop) liveset]
      [(if ,test ,[c-live*] ,[a-live*]) 
        (Pred test c-live* a-live*)]
      [(begin ,ef* ... ,[liveset]) (Effect* ef* liveset)]
      [(mset! ,t1 ,t2 ,t3)
        (let ([lives (union (list t1 t2 t3) liveset)])
          (add-conflicts t1 lives)
          (add-conflicts t2 lives)
          (add-conflicts t3 lives)
          lives)]
      [(return-point ,lbl ,[Tail -> t-liveset]) 
        t-liveset]
      [(set! ,lhs (,binop ,[Triv -> x-liveset] ,[Triv -> y-liveset]))
        (add-conflicts lhs liveset)
        (union x-liveset y-liveset (remove lhs liveset))]
      [(set! ,lhs ,[Triv -> rhs])
        (add-conflicts lhs liveset)
        (union rhs (remove lhs liveset))]))
  (define (Pred expr t-liveset f-liveset)
    (match expr
      [(true) (union t-liveset f-liveset)]
      [(false) (union t-liveset f-liveset)]
      [(if ,test ,[c-liveset] ,[a-liveset])
        (Pred test c-liveset a-liveset)]
      [(begin ,ef* ... ,[liveset]) (Effect* ef* liveset)]
      [(,relop ,[Triv -> x-liveset] ,[Triv -> y-liveset])
        (add-conflicts x-liveset y-liveset)
        (add-conflicts y-liveset x-liveset)
        (union  x-liveset y-liveset t-liveset f-liveset)]))
  (define (Tail expr)
    (match expr
      [(begin ,ef* ... ,[liveset]) 
        (Effect* ef* liveset)]
      [(if ,test ,[Tail -> c-liveset] ,[Tail -> a-liveset]) 
        (Pred test c-liveset a-liveset)]
      [(ccall ,[Triv -> reg] ,lbl ,liveset ...) 
        (add-conflicts reg liveset) 
        (remove reg liveset)]
      ;;  (append reg `(,liveset ...))]
      [(,[Triv -> target-liveset] ,liveset ...)
        (add-conflicts target-liveset liveset)
        (append target-liveset `(,liveset ...))]))
  (begin
    (Tail expr)
    graph))

;; +-------------------------------------------------------------------------+
;; | PASS    | finalize-frame-locations                                      |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (finalize-frame-locations expr)                               |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with frame locations inlined.                  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass takes each of the frame assignments and inlines each. Also,   |
;; | any nops are removed.                                                   |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | A tree walk is performed using an environment holding the frame         |
;; | locations. After that, a tree walk finds each nop and removes it.       |
;; +-------------------------------------------------------------------------+
;; | INPUT / OUTPUT GRAMMAR                                                  |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar *)                                             |
;; |              (ulocals ()                                                |
;; |                (locate ()                                               |
;; |                  (frame-conflict ConflictGraph Tail))))                 |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | CGraph  -> ((uvar Var*) ...)                                            |
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
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+
(define-who finalize-frame-locations
  (define (finalize exp env)
    (cond
      [(and (atom? exp) (uvar? exp))
        (let ([r (assq exp env)])
          (if r (cadr r) exp))]
      [(atom? exp) exp]
      [else
        (cons (finalize (car exp) env)
              (finalize (cdr exp) env))]))
  (define (optimize-nops exp)
    (match exp
      [,x (guard (atom? x)) x]
      [(set! ,a ,b) (guard (eq? a b)) '(nop)]
      [(,a . ,d) (cons (optimize-nops a) (optimize-nops d))]))
  (define (Body expr)
    (match expr
      [(locals (,var* ...) 
         (ulocals (,uvar* ...)
           (locate ([,floc ,fvar] ...)
             (frame-conflict (,frame-cgraph ...) ,tail))))
       `(locals (,var* ...)
          (ulocals (,uvar* ...)
            (locate ([,floc ,fvar] ...)
              (frame-conflict (,frame-cgraph ...) 
                ,(optimize-nops (finalize tail `((,floc ,fvar) ...)))))))]
      [(locate ([,uvar ,Loc] ...) ,tail) 
        `(locate ([,uvar ,Loc] ...) 
                 ,(optimize-nops (finalize tail `((,uvar ,Loc) ...))))]))
  (lambda (expr)
    (match expr
      [(letrec 
        ([,label (lambda () ,[Body -> body*])] ...) ,[Body -> body])
        `(letrec ([,label (lambda () ,body* )] ...) ,body)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | select-instructions                                           |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (select-instructions expr)                                    |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with additional unspillables and modified      |
;; |         | expressions that conform to x86-64 instruction requirements.  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass coerces the input program into x86-64 architecture rules. It  |
;; | is one of the 'black magic' passes of the compiler - it is messy and it |
;; | cannot be cleaned up. But it must be done, so INTO THE BREACH!          |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | A lot of case-by-case 'paperwork' is done, checking and rewriting the   |
;; | expressions to ensure that they conform.                                |
;; +-------------------------------------------------------------------------+
;; | GRAMMAR CHANGE                                                          |
;; +-------------------------------------------------------------------------+
;; | The output-language grammar for this pass is identical to the input     |
;; | language grammar, but all of the architecture restrictions that we      |
;; | dropped from our source language from the preceding assignment are back |
;; | after this pass.                                                        |
;; +-------------------------------------------------------------------------+
;; | INPUT / OUTPUT GRAMMAR                                                  |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar *)                                             |
;; |              (ulocals ()                                                |
;; |                (locate ()                                               |
;; |                  (frame-conflict ConflictGraph Tail))))                 |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | Body    -> (locals (uvar*) (frame-conflict ConflictGraph Tail)          |
;; | CGraph  -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who select-instructions
  (define (Body expr)
    (define new-vars '())
    (define (new-var)
      (let ([u (unique-name 't)])
           (set! new-vars (cons u new-vars))
           u))
    (define (invert op)
      (cond
        [(eq? op '<) '>] [(eq? op '>) '<] [(eq? op '=) '=]
        [(eq? op '<=) '>=] [(eq? op '>=) '<=]))
    (define (Tail tail)
      (match tail
        [(if ,[Pred -> p] ,[Tail -> t1] ,[Tail -> t2]) `(if ,p ,t1 ,t2)]
        [(begin ,[Effect -> e*] ... ,[Tail -> t]) (make-begin `(,e* ... ,t))]
        [(ccall ,reg ,lbl ,loc* ...) `(ccall ,reg ,lbl ,loc* ...)]
        [(,triv ,loc* ...) `(,triv ,loc* ...)]))
    (define (Pred p)
      (match p
        [(true) '(true)]
        [(false) '(false)]
        [(,rop ,t1 ,t2) (guard (relop? rop))
          (relop `(,rop ,t1 ,t2))]
        [(if ,[Pred -> p1] ,[Pred -> p2] ,[Pred -> p3]) 
         `(if ,p1 ,p2 ,p3)]
        [(begin ,[Effect -> e*] ... ,[Pred -> p]) (make-begin `(,e* ... ,p))]))
    (define (relop expr)
      (match expr
        [(,rop ,t1 ,t2)
          (cond
            [(or (uvar? t1) (register? t1) (frame-var? t1)) (relop2 expr)]
            [(or (uvar? t2) (register? t2) (frame-var? t2))
             (relop2 `(,(invert rop) ,t2 ,t1))]
            [else
              (let ([u (new-var)])
                `(begin (set! ,u ,t1) ,(relop2 `(,rop ,u ,t2))))])]))
    (define (relop2 expr)
      (match expr
        [(,rop ,t1 ,t2)
          (cond
            [(and
               (or (uvar? t1) (register? t1))
               (or (and (int64? t2) (not (int32? t2))) (label? t2)))
             (let ([u (new-var)])
               `(begin (set! ,u ,t2) (,rop ,t1 ,u)))]
            [(and (frame-var? t1)
               (or (frame-var? t2) (label? t2) 
                   (and (int64? t2) (not (int32? t2)))))
             (let ([u (new-var)])
               `(begin (set! ,u ,t2) (,rop ,t1 ,u)))]
            [else expr])]))
    (define (Effect expr)
      (match expr
        [(nop) '(nop)]
        [(set! ,v (,op ,t1 ,t2)) (binop1 `(set! ,v (,op ,t1 ,t2)))]
        [(set! ,v ,t)
          (if (and (frame-var? v) 
               (or (frame-var? t) (label? t) (and (int64? t) (not (int32? t)))))
            (let ([u (new-var)])
              `(begin (set! ,u ,t) (set! ,v ,u)))
            `(set! ,v ,t))]
        [(mset! ,t1 ,t2 ,t3)
          (define sets '())  
          (let
            ([v1 
              (cond
                [(or (uvar? t1) (register? t1)) t1]
                [(or (integer? t1) (frame-var? t1) (label? t1))
                  (let ([v (new-var)])
                    (set! sets (cons `(set! ,v ,t1) sets))
                    v)])]
             [v2
              (cond
                [(or (register? t2) (uvar? t2)) t2]
                [(and (integer? t2) (int32? t2)) t2]
                [(or
                  (and (integer? t2) (int64? t2) (not (int32? t2)))
                  (frame-var? t2)
                  (label? t2))
                  (let ([v (new-var)])
                    (set! sets (cons `(set! ,v ,t2) sets))
                    v)])]
             [v3 
              (cond
                [(or (register? t3) (uvar? t3)) t3]
                [(and (integer? t3) (int32? t3)) t3]
                [(or
                  (and (integer? t3) (int64? t3) (not (int32? t3)))
                  (frame-var? t3)
                  (label? t3))
                  (let ([v (new-var)])
                    (set! sets (cons `(set! ,v ,t3) sets))
                    v)])])
            (if (null? sets)
              `(mset! ,v1 ,v2 ,v3)
              `(begin ,sets ... (mset! ,v1 ,v2 ,v3))))]
        [(return-point ,lbl ,[Tail -> t])
          `(return-point ,lbl ,t)]
        [(if ,[Pred -> p] ,[Effect -> e1] ,[Effect -> e2])
         `(if ,p ,e1 ,e2)]
        [(begin ,[Effect -> e*] ... )
          (make-begin `(,e* ...))]))
    (define (binop1 binop)
       (match binop
         [(set! ,v (,op ,t1 ,t2))
           (cond
             [(eq? v t1) (binop2 binop)]
             [(and (or (eq? op '+) (eq? op '*)) (eq? v t2))
               (binop2 `(set! ,v (,op ,t2 ,t1)))]
             [else
               (let ([u (new-var)])
                 (make-begin 
                    `((set! ,u ,t1) 
                      ,(binop2 `(set! ,u (,op ,u ,t2)))
                      (set! ,v ,u))))])]))
    (define (binop2 binop)
      (match binop
        [(set! ,v (,op ,t1 ,t2))
           (cond
             [(and
                (or (eq? op '+) (eq? op '-) (eq? op 'logor) (eq? op 'logand))
                (or
                  (and 
                    (or (uvar? v) (register? v)) 
                    (or (label? t2) (and (int64? t2) (not (int32? t2)))))
                  (and (frame-var? v) 
                    (or (frame-var? t2) (label? t2) 
                      (and (int64? t2) (not (int32? t2)))))))
              (let ([u (new-var)])
                (make-begin `((set! ,u ,t2) (set! ,v (,op ,v ,u)))))]
             [(and (eq? op '*) 
                (or (uvar? v) (register? v))
                (or (label? v) (and (int64? t2) (not (int32? t2)))))
                (let ([u (new-var)])
                  (make-begin `((set! ,u ,t2) (set! ,v (,op ,v ,u)))))]
             [(and (eq? op '*) (frame-var? v))
                (let ([u (new-var)])
                  (make-begin 
                     `((set! ,u ,v) 
                     ,(binop2 `(set! ,u (,op ,u ,t2)))
                     (set! ,v ,u))))]
             [else binop])]))
    (match expr
           [(locals (,local* ...)
                    (ulocals (,ulocal* ...)
                             (locate (,home* ...)
                                     (frame-conflict ,ct ,[Tail -> tail]))))
            `(locals (,local* ...)
                     (ulocals (,ulocal* ... ,new-vars ...)
                              (locate (,home* ...)
                                      (frame-conflict ,ct ,tail))))]
           [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
           [,x (error who "invalid Body ~s" x)]))

  (lambda (expr)
    (match expr
      [(letrec ([,label (lambda () ,[Body -> body*])] ...) ,[Body -> body])
        `(letrec ([,label (lambda () ,body*)] ...) ,body)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | uncover-register-conflict                                     |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (uncover-register-conflict expr)                              |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The original input with conflict graphs listed where the are  |
;; |         | specified by the output grammar.                              |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Searches through the programs, finding all the conflicts that occur in  |
;; | the expressions. Then it puts each of these into the uvar list so that  |
;; | the next few passes can do yummy things in terms of allocating all of   |
;; | those registers.                                                        |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | This sets up the new structure and then calls to do-uncover-conflict    |
;; | with the frame-var? predicate for picking conflicts.                    |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar *)                                             |
;; |              (ulocals ()                                                |
;; |                (locate ()                                               |
;; |                  (frame-conflict ConflictGraph Tail))))                 |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | Body    -> (locals (uvar*) (frame-conflict ConflictGraph Tail)          |
;; | CGraph  -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; |          | (ccall reg label Triv*)                                      |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar *)                                             |
;; |              (ulocals (uvar *)                                          |
;; |                 (locate ([uvar fvar ]*)                                 |
;; |                     (frame-conflict ConflictGraph                       |
;; |                        (register-conflict ConflictGraph Tail)))))       |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; |          | (ccall reg label Triv*)                                      |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who uncover-register-conflict
  (define Body
    (lambda (x)
      (match x
        [(locals (,local* ... )
          (ulocals (,ulocal* ... )
            (locate (,home* ... )
              (frame-conflict ,fv-ct ,tail))))
          (let ([ct (do-uncover-conflict 
                      tail `(,local* ... ,ulocal* ... ) who register?)])
            `(locals (,local* ... )
              (ulocals (,ulocal* ... )
                (locate (,home* ... )
                  (frame-conflict ,fv-ct
                    (register-conflict ,ct ,tail))))))]
        [(locate (,home* ... ) ,tail) `(locate (,home* ... ) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ... ) ,[Body -> body])
        `(letrec ([,label* (lambda () ,body*)] ... ) ,body)]
      [,x (error who "invalid Program ~s" x)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | assign-registers                                              |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (assign-registers expr)                                       |
;; +-------------------------------------------------------------------------+
;; | RETURNS | If successful, it generates a locate form with the register   |
;; |         | assignments, replace the locals and register-conflict forms.  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass attempts to assign register to each uvar in the locals list,  |
;; | using the graph-coloring register assignment algorithm.                 |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | This pass starts by pulling out the conflict graph. Then it does the    |
;; | following:                                                              |
;; |  1) Picks a node and removes it from the tree                           |
;; |     a) Tries to pick one of degree <k, but does what must be done.      |
;; |     b) Gets back the node to remove and if it's a potential spill.      |
;; |        Actual return: ((node conflict*) . [#t|#f - spill])              |
;; |  2) Removes the node from the graph and then recurses.                  |
;; |  3) Gets back the register allocation from the subgraph.                |
;; |  4) Finds out which registers are being used by the nodes that conflict |
;; |     with the one picked for this particular recursion and builds a list |
;; |     of all of the nodes except those.                                   |
;; |  5) Picks the first node off of this list, extends the register list    |
;; |     by the one just allocated, and returns it up to the next step.      |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar *)                                             |
;; |              (ulocals (uvar *)                                          |
;; |                 (locate ([uvar fvar ]*)                                 |
;; |                     (frame-conflict ConflictGraph                       |
;; |                        (register-conflict ConflictGraph Tail)))))       |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
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
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (ulocals (uvar*)                                           |
;; |                (spills (uvar*)                                          |
;; |                 (locate ([uvar fvar]*)                                  |
;; |                     (frame-conflict ConflictGraph Tail )))))            |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
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
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who assign-registers
  (let ((registers (remq 'rsp registers)))
    (define (find-node graph k) 
      (cond
        [(null? graph) '()]
        [else
         (let ([n-k (sub1 (length (car graph)))])
          (if (< n-k k) (car graph)
             (let ([recur (find-node (cdr graph) k)])
               (if (null? recur) (car graph) recur))))]))
    (define (remove-entry node graph)
      (cond
        [(null? graph) '()]
        [(eq? node (caar graph)) (cdr graph)]
        [else (cons (car graph) (remove-entry node (cdr graph)))]))
    (define (remove-node node graph)
      (let ([rgraph (remove-entry node graph)])
        (map (lambda (e) (remove node e)) rgraph)))
    (define (remove-spill-select current-graph full-graph regs k)
      (cond
       [(null? current-graph) '()]
       [else
         (let* 
           ([to-remove (find-node current-graph k)]
            [node      (car to-remove)]
            [conflicts (cdr to-remove)]
            [newgraph  (remove-node node current-graph)]
            [recur     (remove-spill-select newgraph full-graph regs k)]
            [spills    
              (map car (filter (lambda (e) (eq? (cadr e) 'spill)) recur))]
            [new-conflicts 
              (filter (lambda (e) (not (memq e spills))) conflicts)]
            [conflict-regs 
              (fold-right cons '() 
                (map cadr 
                  (filter (lambda (e) (memq (car e) new-conflicts)) recur)))]
            [possible-regs 
              (filter (lambda (e) (not (memq e conflict-regs))) regs)]
            [final-regs 
              (filter (lambda (e) (not (memq e conflicts))) possible-regs )])
           (if (null? final-regs)
             `((,node spill) . ,recur)
             `((,node ,(car final-regs)) . ,recur)))]))
    (define (assign graph regs k)
      (let*
        ([alloc (remove-spill-select graph graph regs k)]
         [spills
           (filter (lambda (e) (eq? (cadr e) 'spill)) alloc)])
        (if (null? spills)
            (values #f alloc)
            (values #t (map car spills)))))
    (define (Body expr)
      (match expr
        [(locals (,var* ...) 
           (ulocals (,uvar* ...)
             (locate ([,floc ,fvar] ...)
                (frame-conflict (,frame-cgraph ...)
                  (register-conflict (,reg-cgraph ...) ,tail)))))
          (let-values
            ([(spills? var-list) 
              (assign reg-cgraph registers (length registers))])
            (if spills?
              (if (null? (intersection var-list uvar*))
                `(locals (,var* ...)
                   (ulocals (,uvar* ...)
                     (spills ,var-list
                       (locate ([,floc ,fvar] ...)
                         (frame-conflict (,frame-cgraph ...) ,tail)))))
                 (errorf who "Spilled an unspillable - ABANDON SHIP\n"))
              `(locate ,(append `((,floc ,fvar) ...) var-list) ,tail)))]
        [(locate ([,uvar ,Loc] ...) ,tail) expr]))
    (lambda (expr)
      (match expr
        [(letrec ([,labels (lambda () ,[Body -> body*])] ... ) ,[Body -> body])
         `(letrec ([,labels (lambda () ,body*)] ...) ,body)]))))

;; +-------------------------------------------------------------------------+
;; | PASS    | everbody-home?                                                |
;; | AUTHOR  | Anonymous - From Compiler Frame                               |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (everybody-home? expr)                                        |
;; +-------------------------------------------------------------------------+
;; | RETURNS | #t if there are every uvar has a reg or fv and #f if not.     |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Ensures that all variables are dealt with (allocated or on the stack).  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Each binding form is checked to ensure that it's correct.               |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (ulocals (uvar*)                                           |
;; |                (spills (uvar*)                                          |
;; |                 (locate ([uvar fvar]*)                                  |
;; |                     (frame-conflict ConflictGraph Tail )))))            |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (ulocals (uvar*)                                           |
;; |                (spills (uvar*)                                          |
;; |                 (locate ([uvar fvar]*)                                  |
;; |                     (frame-conflict ConflictGraph Tail )))))            |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who everybody-home?
  (define all-home?
    (lambda (body)
      (match body
	[(locals (,local* ...)
	   (ulocals (,ulocal* ...)
	     (spills (,spill* ...)
	       (locate (,home* ...)
		 (frame-conflict ,ct ,tail))))) #f]
	[(locate (,home* ...) ,tail) #t]
	[,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
       [(letrec ([,label* (lambda () ,body*)] ...) ,body)
	(andmap all-home? `(,body ,body* ...))]
       [,x (error who "invalid Program ~s" x)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | assign-frame                                                  |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (assign-frame expr)                                           |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with spilled variables placed into frames.     |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass places spilled variables onto the stack frame.                |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | The grammar is walked. Each spill has a new frame location computed for |
;; | it and then hte locate form is updated to reflect that.                 |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (ulocals (uvar*)                                           |
;; |                (spills (uvar*)                                          |
;; |                 (locate ([uvar fvar]*)                                  |
;; |                     (frame-conflict ConflictGraph Tail )))))            |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar *)                                             |
;; |              (ulocals (uvar *)                                          |
;; |                (locate ([uvar fvar ]*)                                  |
;; |                  (frame-conflict conflict-graph Tail))))                |
;; |          | (locate ([uvar Loc]*) Tail )                                 |
;; | C-Graph -> ((uvar Var*) ...)                                            |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who assign-frame
  (define (assign-frames-helper max spills)
    (cond
      [(null? spills) '()]
      [else
        (cons `(,(car spills) ,(index->frame-var (add1 max)))
              (assign-frames-helper (add1 max) (cdr spills)))]))
  (define (assign-frames used conflicts spills)
    (let* ([conflict-frames (fold-right union '() conflicts)]
           [used-frames 
             (filter frame-var? 
                     (union (map cadr used) conflict-frames))]
           [max-val 
             (fold-right max 0 (map frame-var->index used-frames))])
      (append used (assign-frames-helper max-val spills))))
  (define (Body expr)
    (match expr
      [(locals (,var* ...) 
         (ulocals (,uvar* ...)
           (spills (,svar* ...)
             (locate ([,floc ,fvar] ...)
                (frame-conflict (,frame-cgraph ...) ,tail)))))
        (let ([locates (assign-frames `((,floc ,fvar) ...) 
                                      `(,frame-cgraph ...)
                                      `(,svar* ...))])
          `(locals (,var* ...)
             (ulocals (,uvar* ...)
               (locate ,locates
                 (frame-conflict (,frame-cgraph ...) ,tail)))))]
      [(locate ([,uvar ,Loc] ...) ,tail) expr]))
  (lambda (expr)
    (match expr
      [(letrec ([,labels (lambda () ,[Body -> body*])] ... ) ,[Body -> body])
       `(letrec ([,labels (lambda () ,body*)] ...) ,body)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | protect-ccall-lives                                           |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (protect-ccall-lives expr)                                    |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with all live registers at a ccall saved.      |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | I really didn't want to do this. It happened, and is unfortunate.       |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Walks through the forms, looking for locate forms. It carries along the |
;; | registers named there, and when it finds a ccall form it will save them |
;; | onto the stack before the call and restore them afterwards.             |
;; +-------------------------------------------------------------------------+
;; | INPUT/OUTPUT GRAMMAR                                                    |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locate ([uvar Loc]*) Tail )                                 |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
; +-------------------------------------------------------------------------+

(define-who protect-ccall-lives
  (define fp frame-pointer-register)
  (define unique-regs 
    (lambda (ls) (apply union (filter (comp register? car) (map cdr ls)))))
  (define max-framevar 
    (lambda (ls) 
      (fold-right 
        (lambda (x m) 
          (cond
            [(frame-var? x)
            (max (frame-var->index x) m)]
            [else m]))
        0 (map cadr ls))))
  (define c+
    (lambda (n) (lambda (m) (+ n m))))
  (define (Program expr)
    (match expr
      [(letrec ([,label (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label (lambda () ,body*)] ...) ,body)]))
  (define (Body expr)
    (match expr
      [(locate ,loc* ,t)
       (let ((uregs (unique-regs loc*))
             (maxfv (max-framevar loc*)))
        `(locate ,loc* ,((Tail maxfv uregs) t)))]))
  (define (Tail maxfv lregs)
    (lambda (expr)
      (match expr
        [(if ,[(Pred maxfv lregs) -> p]
             ,[(Tail maxfv lregs) -> t1] 
             ,[(Tail maxfv lregs) -> t2])  
         `(if ,p ,t1 ,t2)]
        [(begin ,[(Effect maxfv lregs) -> e*] ... ,[(Tail maxfv lregs) -> t])         
         `(begin ,e* ... ,t)]
        [(ccall ,reg ,lbl ,loc* ...) `(ccall ,reg ,lbl ,loc* ...)] 
        [(,triv ,loc* ...) `(,triv)])))
  (define (Pred maxfv lregs)
    (lambda (expr)
      (match expr
        [(true) `(true)]
        [(false) `(false)]
        [(if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3)]
        [(begin ,[(Effect maxfv lregs) -> e*] ... ,[p]) `(begin ,e* ... ,p)]
        [(,op ,t1 ,t2) `(,op ,t1 ,t2)])))
  (define (Effect maxfv lregs)
    (lambda (expr)
      (match expr
        [(nop) `(nop)]
        [(if ,[(Pred maxfv lregs) -> p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2)]
        [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
        [(return-point ,lbl (begin ,e* ... (ccall ,reg ,call)))
         (let* ((len (length lregs))
                (fvregs (zip lregs 
                             (map (comp index->frame-var (c+ maxfv)) (iota len))))
                (saves (map (lambda (e) `(set! ,(cdr e) ,(car e))) fvregs))
                (restores (map (lambda (e) `(set! ,(car e) ,(cdr e))) fvregs))
                (e* (map (Effect (+ maxfv len) lregs) e*)))
           `(begin 
              ,saves ... 
              (set! ,fp (+ ,fp ,(ash len word-shift)))
              (return-point ,lbl 
                (begin ,e* ... (ccall ,reg ,call)))
              (set! ,fp (- ,fp ,(ash len word-shift)))
              ,restores ...
              ))]
        [(return-point ,lbl ,[(Tail maxfv lregs) -> t]) `(return-point ,lbl ,t)]
        [(set! ,v ,t) `(set! ,v ,t)]
        [(set! ,v (,op ,t1 ,t2)) `(set! ,v (,op ,t1 ,t2))]
        [(mset! ,t1 ,t2 ,t3) `(mset! ,t1 ,t2 ,t3)])))
  (lambda (expr)
    (Program expr)))

;; +-------------------------------------------------------------------------+
;; | PASS    | discard-call-live                                             |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (discard-call-live expr)                                      |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression passed in with all lives in a call removed.    |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass simply discards each of the call-live lists.                  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Walks down to Tail, looks around for any call live forms, and removes   |
;; | the lives from any call it finds. Then it just yankee-doodle-dandies on |
;; | its way through the rest of the bodies, repeating this step.            |
;; |                                                                         |
;; | The tree-walk attempt did not quite work out and now I process each of  |
;; | Effect and Pred expressions.                                            |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locate ([uvar Loc]*) Tail )                                 |
;; | Tail    -> (Triv Loc*)                                                  |
;; |          | (ccall reg label Triv*)                                      |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Triv Triv)                                            |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; | Effect  -> (nop)                                                        |
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locate ([uvar Loc]*) Tail )                                 |
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
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who discard-call-live
  (define (Program expr)
    (match expr
      [(letrec ([,label (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label (lambda () ,body*)] ...) ,body)]))
  (define (Body expr)
    (match expr
      [(locate ,loc* ,[Tail -> t])
        `(locate ,loc* ,t)]))
  (define (Tail expr)
    (match expr
      [(if ,[Pred -> p] ,[Tail -> t1] ,[Tail -> t2])  `(if ,p ,t1 ,t2)]
      [(begin ,[Effect -> e*] ... ,[Tail -> t])         `(begin ,e* ... ,t)]
      [(ccall ,reg ,lbl ,loc* ...) `(ccall ,reg ,lbl ,loc* ...)] 
      [(,triv ,loc* ...) `(,triv)]))
  (define (Pred expr)
    (match expr
      [(true) `(true)]
      [(false) `(false)]
      [(if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3)]
      [(begin ,[Effect -> e*] ... ,[p]) `(begin ,e* ... ,p)]
      [(,op ,t1 ,t2) `(,op ,t1 ,t2)]))
  (define (Effect expr)
    (match expr
      [(nop) `(nop)]
      [(if ,[Pred -> p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2)]
      [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)] 
      [(return-point ,lbl ,[Tail -> t]) `(return-point ,lbl ,t)]
      [(set! ,v ,t) `(set! ,v ,t)]
      [(set! ,v (,op ,t1 ,t2)) `(set! ,v (,op ,t1 ,t2))]
      [(mset! ,t1 ,t2 ,t3) `(mset! ,t1 ,t2 ,t3)]))
  (lambda (expr)
    (Program expr)))

;; +-------------------------------------------------------------------------+
;; | PASS    | finalize-locations                                            |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (finalize-locations expr)                                     |
;; +-------------------------------------------------------------------------+
;; | RETURNS | Returns the tree with the location structure removed and all  |
;; |         | location variables replaced with the regs or fvars they mean. |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass replaces each occurance of a 'uvar' in the body of each       |
;; | 'locate' with the corresponding location. It also kills all usesless    |
;; | commands and replaces them with (nop) - so (set! rax rax) -> nop.       |
;; | It also discards the 'locate' form.                                     |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | It does both of these with simple tree walks. It first walks, swapping  |
;; | out the uvars with their associates. It then walks around and finds any |
;; | and all nop-friendly statements and removes them. I may add a few more  |
;; | optimizations here as time goes on. Here is a current list of them:     |
;; | - Removes any statement (set! a a)                                      |
;; | - Removes any statement (set! a (+ a 0))                                |
;; | - Removes any statement (set! a (* a 1))                                |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locate ([uvar Loc]*) Tail)                                  |
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
;; |          | (set! Var Triv)                                              |
;; |          | (set! Var (binop Triv Triv))                                 |
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point lbl Tail)                                      |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
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

(define-who finalize-locations
  (define (finalize expr)
    (match expr
      [(locate ([,uvar ,loc] ...) ,body)
        (optimize-nops 
          (replace-locs-with-vars body (zip uvar loc)))]))
  (define (replace-locs-with-vars exp env)
    (cond
      [(and (atom? exp) (uvar? exp))
        (let ([r (assq exp env)])
          (if r
            (cdr r)
            (errorf who "undefined unique variable ~s" exp)))]
      [(atom? exp) exp]
      [else
        (cons (replace-locs-with-vars (car exp) env)
        (replace-locs-with-vars (cdr exp) env))]))
  (define (optimize-nops exp)
    (match exp
      [,x (guard (atom? x)) x]
      [(set! ,a ,b) (guard (eq? a b)) '(nop)]
      [(set! ,a (+ ,b 0)) '(nop)]
      [(set! ,a (* ,b 1)) '(nop)]
      [(,a . ,d)
        (cons (optimize-nops a) (optimize-nops d))]))
  (lambda (expr)
    (match expr
      [(letrec 
        ([,label (lambda () ,[finalize -> Body*])] ...) ,[finalize -> Body])
        `(letrec ([,label (lambda () ,Body* )] ...) ,Body)])))

) ;;end library
