(library (compiler cform-introduction)
  (export introduce-cform)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | introduce-cform                                               |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (introduce-cform expr)                                        |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with FFI calls and other C language calls      |
;; |         | replaced with cform expressions.                              |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass removes any FFI and FFI-like system calls that the language   |
;; | provides, replacing them with the cform syntax, which takes a call to   |
;; | perform (including values as arguments), a procedure name to call in    | 
;; | the resulting C code, and a register to store the result in.            | 
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | It uses a tree walk, which replaces anything in the cform environment   |
;; | (and later the FFI interface we provide to the users) with the cform    |
;; | language form. This gives us the added bonus that we can implement our  |
;; | collector calls.                                                        |
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
;; |          | (display <uvar|int>)                                         |
;; |          | (newline)                                                    |
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

(define-who (introduce-cform expr)
  (define ccalls '(display newline))
  (define (setup-call expr)
    (match expr
      [(display ,x) `(begin (ccall rax "print1" 0 ,x))]
      [(newline) `(begin (set! rax 0) (ccall rax "printf" "\n"))]))
  (define (walk expr)
    (match expr
      [,x (guard (atom? x)) x]
      [(,e ,[x] ...) (guard (memq e ccalls)) (setup-call `(,e ,x ...))]
      [(,[a] . ,[d]) `(,a . ,d)]))
  (define (Body expr)
    (match expr
      [(locals ,uvars ,[walk -> t]) 
       `(locals ,uvars ,t)]))
  (match expr
    [(letrec ([,l (lambda ,args ,[Body -> b*])] ...) ,[Body -> b])
      `(letrec ([,l (lambda ,args ,b*)] ...) ,b)]))

;; How I would do this if I wasn't lazy: -- Cam
;;  (define (Effect expr)
;;     (match expr
;;       [(return-point ,lbl ,[Tail -> t]) `(return-point ,lbl ,t)]
;;       [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
;;       [(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
;;       [(,e ,[x] ...) (guard (assq e cenv))
;;         (let-values 
;;           (((reg lbl) (match (assq e cenv) [(,name ,reg ,lbl) (values reg lbl)])))
;;           `(ccall ,reg ,lbl ,x ...))]
;;       [,x x]))
;;   (define (Pred expr)
;;     (match expr
;;       [(true) '(true)]
;;       [(false) '(false)]
;;       [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
;;       [(begin ,[Effect -> e*] ... ,[p]) (make-begin `(,e* ... ,p))]
;;       [(,relop ,t ,t) (guard (memq relop relops)) `(relop ,t ,t)]))
;;   (define (Tail expr)
;;     (match expr
;;       [(begin ,[Effect -> e*] ... ,[t]) (make-begin `(,e* ... ,t))]
;;       [(if ,[Pred -> test] ,[Pred -> conseq] ,[Pred -> alt])
;;        `(if ,test ,conseq ,alt)]
;;       [(,t ,l* ...) `(,t ,l* ...)]
;;       [,t t])) (define (Effect expr)
;;     (match expr
;;       [(return-point ,lbl ,[Tail -> t]) `(return-point ,lbl ,t)]
;;       [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
;;       [(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
;;       [(,e ,[x] ...) (guard (assq e cenv))
;;         (let-values 
;;           (((reg lbl) (match (assq e cenv) [(,name ,reg ,lbl) (values reg lbl)])))
;;           `(ccall ,reg ,lbl ,x ...))]
;;       [,x x]))

)
