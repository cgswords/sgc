(library (compiler flatten-let)
  (export 
    uncover-locals
    remove-let)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | uncover-locals                                                |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (uncover-locals expr)                                         |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with a locals form added. The locals form      |
;; |         | contains every variable bound in a let.                       |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass finds any and every variable bound in a let and adds that     |
;; | variable to a locals form wrapping each body.                           |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | A tree walk is performed to build a list of local variables for each    |
;; | body. Then each list is added to the locals form and the expression is  |
;; | handed back.                                                            |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Tail)]*) Tail)               |
;; | Tail    -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; |          | (let ([uvar Value]*) Tail)                                   |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Value Value)                                          |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; |          | (let ([uvar Value]*) Pred)                                   |
;; | Effect  -> (nop)                                                        |
;; |          | (mset! Value Value Value)                                    |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (let ([uvar Value]*) Effect)                                 |
;; | Value   -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; |          | (let ([uvar Value]*) Value)                                  |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; |          | (let ([uvar Value]*) Tail)                                   |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Value Value)                                          |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; |          | (let ([uvar Value]*) Pred)                                   |
;; | Effect  -> (nop)                                                        |
;; |          | (mset! Value Value Value)                                    |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (let ([uvar Value]*) Effect)                                 |
;; | Value   -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; |          | (let ([uvar Value]*) Value)                                  |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+

(define-who (uncover-locals expr)
  (define (Program expr)
    (match expr
      [(letrec ([,l* (lambda ,uvar* ,b*)] ...) ,b)
        (let ([locals* (map locals b*)])
        `(letrec 
           ([,l* (lambda ,uvar* (locals ,locals* ,b*))] ...) 
           (locals ,(locals b) ,b)))]))
  (define (locals expr)
    (define new-lives '())
    (define (walk expr)
      (match expr
        [,x (guard (atom? x)) x]
        [(let ([,x* ,[e*]] ...) ,[body])
          (set! new-lives (union x* new-lives))
          expr]
        [(,[a] . ,[d])
          `(,a . ,d)]))
    (walk expr)
    new-lives)
  (Program expr))


;; +-------------------------------------------------------------------------+
;; | PASS    | remove-let                                                    |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (remove-let expr)                                             |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with all let forms turned into set!s.          |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass removes the let form by replacing each [x e] into (set! x e). |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | A tree walk finds let expressions and performs the substitutions.       |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; |          | (let ([uvar Value]*) Tail)                                   |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Value Value)                                          |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; |          | (let ([uvar Value]*) Pred)                                   |
;; | Effect  -> (nop)                                                        |
;; |          | (mset! Value Value Value)                                    |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (let ([uvar Value]*) Effect)                                 |
;; | Value   -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; |          | (let ([uvar Value]*) Value)                                  |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) Tail)                                        |
;; | Tail    -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Tail Tail)                                          |
;; |          | (begin Effect* Tail)                                         |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (relop Value Value)                                          |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; |          | (let ([uvar Value]*) Pred)                                   |
;; | Effect  -> (nop)                                                        |
;; |          | (mset! Value Value Value)                                    |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (let ([uvar Value]*) Effect)                                 |
;; | Value   -> Triv                                                         |
;; |          | (binop Value Value)                                          |
;; |          | (alloc Value)                                                |
;; |          | (Value Value*)                                               |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; |          | (let ([uvar Value]*) Value)                                  |
;; | Triv    -> uvar | int | label                                           |
;; +-------------------------------------------------------------------------+

(define-who (remove-let expr)
  (define (Program expr)
    (match expr
      [(letrec ([,l* (lambda ,uvar* (locals ,loc* ,[Body -> b*]))] ...) (locals ,loc ,[Body -> b]))
        `(letrec 
           ([,l* (lambda ,uvar* (locals ,loc* ,b*))] ...) 
           (locals ,loc ,b))]))
  (define (Body expr)
    (define (walk expr)
      (match expr
        [,x (guard (atom? x)) x]
        [(let ([,x* ,[e*]] ...) ,[body])
          (let ([sets (map (lambda (x e) `(set! ,x ,e)) x* e*)])
              (make-begin `(,sets ... ,body)))]
        [(,[a] . ,[d])
          `(,a . ,d)]))
    (walk expr))
  (Program expr))


)
