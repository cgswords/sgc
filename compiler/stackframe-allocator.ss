(library (compiler stackframe-allocator)
         (export
          uncover-frame-conflict
          pre-assign-frame
          assign-new-frame)
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
;; | Uncovers conflicts for anything that passes the test predicate passed.  |
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
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> uvar | int | label                                           |
;; | Frame   -> (uvar*)                                                      |
;; +-------------------------------------------------------------------------+

(define (do-uncover-conflict expr uvar* who test)
  (define graph '())
  (define call-lives '())
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
      [(return-point ,lbl ,[Tail -> t])
        (set! call-lives (union liveset call-lives))      
        (union t liveset)]
      [(set! ,lhs (,binop ,[Triv -> x-liveset] ,[Triv -> y-liveset]))
        (add-conflicts lhs liveset)
        (union x-liveset y-liveset (remove lhs liveset))]
      [(mset! ,t1 ,t2 ,t3)
        (let ([lives (union (list t1 t2 t3) liveset)])
          (add-conflicts t1 lives)
          (add-conflicts t2 lives)
          (add-conflicts t3 lives)
          lives)]
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
      [(if ,test ,[c-liveset] ,[a-liveset]) 
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
    (values call-lives graph)))

;; +-------------------------------------------------------------------------+
;; | PASS    | uncover-frame-conflict                                        |
;; | AUTHOR  | Anonymous - From Compiler Framework                           |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (uncover-frame-conflict expr)                                 |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with the added (frame-conflict ...) form.      |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass records for each variable the set of other variables and      |
;; | frame locations (frame variables) with which it conflicts. This pass    |
;; | and uncover-register-conflict are similar. The only difference with     |
;; | respect to the handling of body expressions is that where               |
;; | uncover-register-conflict looks for registers with register?,           |
;; | uncover-frame-conflict looks for frame variables using frame-var?       |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | This sets up the new structure and then calls to do-uncover-conflict    |
;; | with the frame-var? predicate for picking conflicts.                    |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
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
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> uvar | int | label                                           |
;; | Frame   -> (uvar*)                                                      |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (new-frames (Frame*)                                       |
;; |                (spills (uvar*)                                          |
;; |                  (frame-conflict ConflictGraph                          |
;; |                    (call-live ([uvar|fvar]*) Tail))))                   |
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
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who uncover-frame-conflict
  (define Body
    (lambda (x)
      (match x
        [(locals (,uvar* ... ) (new-frames (,Frame* ...) ,tail))
          (let-values 
            ([(cl-reg ct) (do-uncover-conflict tail uvar* who frame-var?)])
            (let* 
              ([call-live*
                 (filter (lambda (e) (or (uvar? e) (frame-var? e))) cl-reg)]
               [spill* (filter uvar? call-live*)])
              `(locals (,uvar* ...)
                 (new-frames (,Frame* ...)
                   (spills (,spill* ...)
                     (frame-conflict ,ct
                       (call-live (,call-live* ...) ,tail)))))))]
        [,x (error who "invalid Body ~s" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ... ) ,[Body -> body])
        `(letrec ([,label* (lambda () ,body*)] ... ) ,body)]
      [,x (error who "invalid Program ~s" x)])))

;; +-------------------------------------------------------------------------+
;; | PASS    | pre-assign-frame                                              |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (pre-assign-frame expr)                                       |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression, prepped for assignments.                      |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Introduces the locate form for frame variables.                         |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | A generic tree walk inserts the form where necessary.                   |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (new-frames (Frame*)                                       |
;; |                (spills (uvar*)                                          |
;; |                  (frame-conflict ConflictGraph                          |
;; |                    (call-live ([uvar|fvar]*) Tail))))                   |
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
;; |          | (return-point label Tail)                                    |
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
;; |              (new-frames (Frame*)                                       |
;; |                (locate ([uvar fvar]*)                                   |
;; |                  (frame-conflict ConflictGraph                          |
;; |                    (call-live ([uvar|fvar]*) Tail))))                   |
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
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who pre-assign-frame
  (define (assign-frames-helper max-var spills)
    (cond
      [(null? spills) '()]
      [else
        (cons `(,(car spills) ,(index->frame-var max-var))
              (assign-frames-helper (add1 max-var) (cdr spills)))]))
  (define (assign-frames conflicts spills)
    (let* ([conflict-frames (fold-right union '() conflicts)]
           [max-val 
             (fold-right max -1 (map frame-var->index (filter frame-var? conflict-frames)))])
      (assign-frames-helper (add1 max-val) spills)))
  (define (Body expr)
    (match expr
      [(locals (,var* ...) 
        (new-frames (,nfv* ...)
          (spills (,svar* ...) 
            (frame-conflict (,frame-cgraph ...) 
              (call-live (,cvar* ...) ,tail)))))
        (let ([locates (assign-frames `(,frame-cgraph ...)
                                      `(,svar* ...))])
          `(locals (,var* ...)
            (new-frames (,nfv* ...)
              (locate ,locates
                (frame-conflict (,frame-cgraph ...)
                  (call-live (,cvar* ...) ,tail))))))]))
  (lambda (expr)
    (match expr
      [(letrec ([,labels (lambda () ,[Body -> body*])] ... ) ,[Body -> body])
       `(letrec ([,labels (lambda () ,body*)] ...) ,body)])))


;; +-------------------------------------------------------------------------+
;; | PASS    | assign-new-frame                                              |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (assign-new-frame expr)                                       |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with call-lives removed and variables located. |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | Anythihing in the call-live form is placed where it needs to be.        |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | The grammar is walked any expression in the binding form is dealt with. |
;; | Also, the return-point form has its frame-pointer introduced here.      |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda () Body)]*) Body)                    |
;; | Body    -> (locals (uvar*)                                              |
;; |              (new-frames (Frame*)                                       |
;; |                (locate ([uvar fvar]*)                                   |
;; |                  (frame-conflict ConflictGraph                          |
;; |                    (call-live ([uvar|fvar]*) Tail))))                   |
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
;; |          | (return-point label Tail)                                    |
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
;; |              (ulocals ()                                                |
;; |                (locate ([uvar fvar]*)                                   |
;; |                  (frame-conflict ConflictGraph Tail))))                 |
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
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Var     -> uvar | Loc                                                   |
;; | Triv    -> Var | int | label                                            |
;; +-------------------------------------------------------------------------+

(define-who assign-new-frame
  (define (Program expr)
    (match expr
      [(letrec ([,lbl (lambda () ,[Body -> b*])] ...) ,[Body -> b])
        `(letrec ([,lbl (lambda () ,b*)] ...) ,b)]))
  (define (Body expr)
    (define frame-size 0)
    (define fp frame-pointer-register)
    (define nfv-locs '())
    (define (locate-new-frames vars f-index)
      (cond
        [(null? vars) '()]
        [else (cons `(,(car vars) ,(index->frame-var f-index))
                (locate-new-frames (cdr vars) (add1 f-index)))]))
    (define (Tail expr)
      (match expr
        [(if ,[Pred -> p] ,[Tail -> t1] ,[Tail -> t2]) `(if ,p ,t1 ,t2)]
        [(begin ,[Effect -> e*] ... ,[Tail -> t]) (make-begin `(,e* ... ,t))]
        [(ccall ,reg ,lbl ,l* ...) `(ccall ,reg ,lbl ,l* ...)]
        [(,t ,l* ...) `(,t ,l* ...)]))
    (define (Pred expr)
      (match expr
        [(true) `(true)]
        [(false) `(false)]
        [(if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3)]
        [(begin ,[Effect -> e*] ... ,[p]) (make-begin `(,e* ... ,p))]
        [(,op ,t1 ,t2) `(,op ,t1 ,t2)]))
    (define (Effect expr)
      (match expr
        [(nop) `(nop)]
        [(if ,[Pred -> p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2)]
        [(begin ,[Effect -> e*] ... ,[e]) (make-begin `(,e* ... ,e))]
        [(set! ,v ,t) `(set! ,v ,t)]
        [(mset! ,t1 ,t2 ,t3) `(mset! ,t1 ,t2 ,t3)]
        [(set! ,v (,op ,t1 ,t2)) `(set! ,v (,op ,t1 ,t2))]
        [(return-point ,lbl ,[Tail -> t])
          `(begin (set! ,fp (+ ,fp ,(ash frame-size word-shift)))
                  (return-point ,lbl ,t)
                  (set! ,fp (- ,fp ,(ash frame-size word-shift))))]))
    (define (lookup var locs)
      (cond
        [(null? locs) (index->frame-var -1)]
        [(eq? var (caar locs)) (cadar locs)]
        [else (lookup var (cdr locs))]))
    (match expr
      [(locals (,uvar* ...)
         (new-frames (,f* ...)
           (locate (,loc* ...)
             (frame-conflict ,cg
               (call-live (,cvar* ...) ,t)))))
       (begin
          (set! frame-size
            (add1
              (fold-right max -1 (map frame-var->index
                                      (map (lambda (e) (lookup e loc*)) cvar*)))))
         (let ([nfv-locs 
                 (apply append
                   (map (lambda (e) (locate-new-frames e frame-size)) `(,f* ...)))]
               [new-locals (difference `(,uvar* ...) 
                                       (fold-right append '() `(,f* ...)))]) 
          `(locals ,new-locals
              (ulocals ()
                (locate (,loc* ... ,nfv-locs ...)
                  (frame-conflict ,cg ,(Tail t)))))))]))
  (lambda (expr) (Program expr)))


) ;; END LIBRARY
