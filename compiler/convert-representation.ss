(library (compiler convert-representation)
  (export 
    lift-letrec
    normalize-context
    specify-representation
    fold-constants)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (compiler helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | lift-letrec                                                   |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (lift-letrec expr)                                            |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The input expression with all letrecs lifted out.             |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass simply moves all letrec bindings from where they appear into  |
;; | a letrec expression wrapped around the outermost expression, removing   |
;; | all of the internal letrec expressions in the process.                  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | This pass walks the input as a tree, accumulating any and all letrecs.  |
;; | It then builds the outer letrec from these accumulated expressions and  |
;; | dumps the rest into the main body of the letrec.                        |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> Expr                                                         |
;; | Expr    -> label                                                        |
;; |          | uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (letrec ([label (lambda (uvar*) Expr)]*) Expr)               |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Expr)]*) Expr)               |
;; | Expr    -> label                                                        |
;; |          | uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+

(define-who (lift-letrec expr)
  (define (Program expr)
    (define new-lets '()) ;; Of the form ((label expr) ...)
    (define (Expr expr)
      (match expr
        [,x (guard (atom? x)) x]
        [(letrec ([,lbl ,[body]] ...) ,[e])
          (set! new-lets (append `([,lbl ,body] ...) new-lets))
          e]
        [(,[a] . ,[d])
          `(,a . ,d)]))
    (let ([e (Expr expr)])
      `(letrec ,new-lets ,e)))
  (Program expr)
)

;; +-------------------------------------------------------------------------+
;; | PASS    | normalize-context                                             |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (normalize-context expr)                                      |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression, modified to conform to the form of the output |
;; |         | grammar (using Value, Pred, and Effect non-terminals).        |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass walks the input grammar, coercing it to match the output      |
;; | grammar by doing minor rewrites to anything that does not already match |
;; | the expected output.                                                    |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | The grammar is walked as if it were in normalize-context form, but each |
;; | line of the input grammar is matched in each case. If at any point the  |
;; | input is not in the right form, the expression is modified to properly  |
;; | conform. This ensures that the grammar fits the proper output. The one  |
;; | thing to note is that some Value expressions are completely removed     |
;; | in Effect syntax because their computations are never used. Even so,    |
;; | this may lead to compiling some code that originally had errors in it.  |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Expr)]*) Expr)               |
;; | Expr    -> label                                                        |
;; |          | uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Expr Expr Expr)                                          |
;; |          | (begin Expr* Expr)                                           |
;; |          | (let ([uvar Expr]*) Expr)                                    |
;; |          | (prim Expr*)                                                 |
;; |          | (Expr Expr*)                                                 |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Value)]*) Value)             |
;; | Value   -> label                                                        |
;; |          | uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; |          | (let ([uvar Value]*) Value)                                  |
;; |          | (value-prim Value*)                                          |
;; |          | (Value Value*)                                               |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; |          | (let ([uvar Value]*) Pred)                                   |
;; |          | (pred-prim Value*)                                           |
;; | Effect  -> (nop)                                                        |
;; |          | (newline)                                                    |
;; |          | (display Value)                                              |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (let ([uvar Value]*) Effect)                                 |
;; |          | (effect-prim Value*)                                         |
;; |          | (Value Value*)                                               |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+

(define-who (normalize-context expr)
  (define (value-prim? op)
    (memq op '(+ - * car cdr cons make-vector vector-length vector-ref void make-procedure procedure-ref procedure-code)))
  (define (pred-prim? op)
    (memq op '(< <= = >= > boolean? eq? fixnum? null? pair? vector? procedure?)))
  (define (effect-prim? op)
    (memq op '(set-car! set-cdr! vector-set! procedure-set! newline display)))
  (define (Program expr)
    (match expr
      [(letrec ([,l* (lambda ,uv* ,[Value -> v*])] ...) ,[Value -> v])
        `(letrec ([,l* (lambda ,uv* ,v*)] ...) ,v)]))
  (define (Value expr)
    (match expr
      [,x 
        (guard (or (uvar? x) (label? x))) x]
      [(quote ,i) 
        (guard (immediate? i)) `(quote ,i)]
      [(if ,[Pred -> p] ,[e1] ,[e2])
        `(if ,p ,e1 ,e2)]
      [(begin ,[Effect -> e*] ... ,[e])
        `(begin ,e* ... ,e)]
      [(let ([,uv ,[e*]] ...) ,[e]) 
        `(let ([,uv ,e*] ...) ,e)]
      [(,vprim ,[e*] ...) 
        (guard (value-prim? vprim)) `(,vprim ,e* ...)]
      [(,pprim ,[e*] ...) 
        (guard (pred-prim? pprim)) `(if (,pprim ,e* ...) '#t '#f)]
      [(,eprim ,[e*] ...) 
        (guard (effect-prim? eprim)) `(begin (,eprim ,e* ...) (void))]
      [(,[e*] ...) 
        `(,e* ...)]))
  (define (Pred expr)
    (match expr
      [,x 
        (guard (or (uvar? x) (label? x))) `(if (eq? ,x '#f) (false) (true))]
      [(quote #t) `(true)]
      [(quote #f) `(false)]
      [(quote ,i) 
        (guard (immediate? i)) `(if (eq? (quote ,i) '#f) (false) (true))]
      [(if ,[p] ,[e1] ,[e2]) 
        `(if ,p ,e1 ,e2)]
      [(begin ,[Effect -> e*] ... ,[e]) 
        `(begin ,e* ... ,e)]
      [(let ([,uv ,[Value -> e*]] ...) ,[e]) 
        `(let ([,uv ,e*] ...) ,e)]
      [(,vprim ,[Value -> e*] ...) 
        (guard (value-prim? vprim))  `(if (eq? (,vprim ,e* ...) '#f) (false) (true))]
      [(,pprim ,[Value -> e*] ...) 
        (guard (pred-prim? pprim))  `(,pprim ,e* ...)]
      [(,eprim ,[Value -> e*] ...) 
        (guard (effect-prim? eprim)) `(begin
                                        (,eprim ,e* ...)
                                        (true))]
      [(,[Value -> e*] ...) 
        `(if (eq? (,e* ...) '#f) (false) (true))]))
  (define (Effect expr)
    (match expr
      [,x 
        (guard (or (uvar? x) (label? x))) '(nop)]
      [(quote ,i)
        (guard (immediate? i)) '(nop)]
      [(if ,[Pred -> p] ,[e1] ,[e2]) 
        `(if ,p ,e1 ,e2)]
      [(begin ,[e*] ... ,[e]) 
        `(begin ,e* ... ,e)]
      [(let ([,uv ,[Value -> e*]] ...) ,[e]) 
        `(let ([,uv ,e*] ...) ,e)]
      [(,vprim ,[e*] ...) 
        (guard (value-prim? vprim)) (make-nopless-begin `(,e* ...))] 
      [(,pprim ,[e*] ...) 
        (guard (pred-prim? pprim))  (make-nopless-begin `(,e* ...))]
      [(,eprim ,[Value -> e*] ...) 
        (guard (effect-prim? eprim)) `(,eprim ,e* ...)]
      [(,[Value -> e*] ...) 
        `(,e* ...)]))
  (Program expr)
)

;; +-------------------------------------------------------------------------+
;; | PASS    | specify-representation                                        |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (specify-representation expr)                                 |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with primitives removed and type tags added.   |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass does several things:                                          |
;; |   1] Converts quoted fixnums and other immediates into unquoted pointer |
;; |      equivalents.                                                       |
;; |   2] Sets up multiplication shifting to deal with type tags properly.   |
;; |   3] Converts calls to cons and make-vector into allocation calls.      |
;; |   4] Calls to car, cdr, vector-length, and vector-ref turn into mref.   |
;; |   5] Converts calls to set-cdr!, set-car!, and vector-set! into mset!.  |
;; |   6] Expands predicates of type boolean?, fixnum?, pair?, and vector?   |
;; |      into calls to logand and =, using appropriate masking for tags.    |
;; |   7] Converts calls to eq? into calls to =. It also converts calls to   |
;; |      null? into calls to = with the empty list as one argument.         |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | This pass is broken up into three passes (for simplicity). The passes   |
;; | are as follows:                                                         |
;; |   1] Walk the expression, handling immediate datatypes.                 |
;; |   2] Walk the expression, handling non-immediate datatypes.             |
;; |   3] Handling type and equivalence tests.                               |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Value)]*) Value)             |
;; | Value   -> label                                                        |
;; |          | uvar                                                         |
;; |          | (quote Immediate)                                            |
;; |          | (if Pred Value Value)                                        |
;; |          | (begin Effect* Value)                                        |
;; |          | (let ([uvar Value]*) Value)                                  |
;; |          | (value-prim Value*)                                          |
;; |          | (Value Value*)                                               |
;; | Pred    -> (true)                                                       |
;; |          | (false)                                                      |
;; |          | (if Pred Pred Pred)                                          |
;; |          | (begin Effect* Pred)                                         |
;; |          | (let ([uvar Value]*) Pred)                                   |
;; |          | (pred-prim Value*)                                           |
;; | Effect  -> (nop)                                                        |
;; |          | (newline)                                                    |
;; |          | (display Value)                                              |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; |          | (let ([uvar Value]*) Effect)                                 |
;; |          | (effect-prim Value*)                                         |
;; |          | (Value Value*)                                               |
;; | Immedi. -> fixnum | () | #t | #f                                        |
;; +-------------------------------------------------------------------------+
;; | OUTPUT GRAMMAR                                                          |
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
;; |          | (newline)                                                    |
;; |          | (display Value)                                              |
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

(define-who (specify-representation expr)
  (define (valop-prim? op)
    (memq op '(+ - * car cdr cons make-vector vector-length vector-ref void make-procedure procedure-ref procedure-code)))
  (define (predop-prim? op)
    (memq op '(< <= = >= > boolean? eq? fixnum? null? pair? vector? procedure?)))
  (define (effectop-prim? op)
    (memq op '(set-car! set-cdr! vector-set! procedure-set! display newline)))
  (define offset-car (- disp-car tag-pair))
  (define offset-cdr (- disp-cdr tag-pair))
  (define offset-vector-length (- disp-vector-length tag-vector))
  (define offset-vector-data (- disp-vector-data tag-vector))
  (define offset-procedure-code (- disp-procedure-code tag-procedure))
  (define offset-procedure-length (- disp-procedure-length tag-procedure))
  (define offset-procedure-data (- disp-procedure-data tag-procedure))
  (define unshift-fixnum (* -1 shift-fixnum))
  (define (predicate? expr)
    (memq expr '(boolean? eq? fixnum? null? pair? vector?)))

  (define (handle-immediate expr)
    (cond
      [(integer? expr) (ash expr shift-fixnum)]
      [(eq? expr '#t) $true]
      [(eq? expr '#f) $false]
      [(eq? expr '()) $nil]
      [(eq? expr 'void) $void]
      [else expr]))
;; This is clearly what should be done: a unified data structure for
;; all memory allocations early on will clean up any number of things.
;; It will also make matching and other noise much, much cleaner.
;; But ain't nobody got time for that.
;;(define (handle-sequence expr)
;;  (define (handle-procedure e)
;;    (match e
;;      [(seq proc ,code ,size) 
;;          (let ([tmp (unique-name 'tmp)])
;;            `(let ([,tmp (+ (alloc ,(+ disp-procedure-data size)) ,tag-procedure)])
;;              (begin
;;                (mset! ,tmp ,offset-procedure-code ,code)
;;                (mset! ,tmp ,offset-procedure-length ,size)
;;                ,tmp)))]))
;;   (define (build-check tag exp)
;;     (match tag
;;       [vector `(if (= (logand ,e1 ,mask-vector) ,tag-vector) 
;;                    (= (logand ,e1 ,mask-fixnum) ,tag-fixnum) ,$false)]
;;       [])) 
;;  (match expr
;;    [(seq-set! ,tag . ,input) ...]
;;    [(seq-ref ,tag . ,input) ...]
;;    [(seq-type? ,tag ,input) (build-check tag input)]
;;    [(seq ,tag . ,input...)
;;     (match tag
;;        [proc (handle-procedure e)]
;;        [string (handle-string e)]
;;        [vector (handle-vector e)])]))

  (define (handle-value-primitive expr)
    (match expr
      [(+ ,e1 ,e2) expr]
      [(- ,e1 ,e2) expr]
      [(* ,x ,y)
        (cond
          [(and (integer? x) (integer? y)) `(* ,x ,(ash y unshift-fixnum))]
          [(integer? x) `(* ,y ,(ash x unshift-fixnum))]
          [(integer? y) `(* ,x ,(ash y unshift-fixnum))]
          [else `(* ,x (sra ,y ,shift-fixnum))])]
      [(car ,e) `(mref ,e ,offset-car)]
      [(cdr ,e) `(mref ,e ,offset-cdr)]
      [(cons ,e1 ,e2)
        (let ([tmp-car (unique-name 'tmp-car)]
              [tmp-cdr (unique-name 'tmp-cdr)]
              [tmp (unique-name 'tmp)])
          `(let ([,tmp-car ,e1] [,tmp-cdr ,e2])
             (let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
               (begin
                 (mset! ,tmp ,offset-car ,tmp-car)
                 (mset! ,tmp ,offset-cdr ,tmp-cdr)
                 ,tmp))))]
      [(make-vector ,e)
        (let ([tmp (unique-name 'tmp)])
          (if (integer? e)
            `(let ([,tmp (+ (alloc ,(+ disp-vector-data e)) ,tag-vector)])
              (begin
                (mset! ,tmp ,offset-vector-length ,e)
                ,tmp))
            (let ([tmp2 (unique-name 'tmp2)])
              `(let ([,tmp ,e])
                (let 
                  ([,tmp2 (+ (alloc (+ ,disp-vector-data ,tmp)) ,tag-vector)])
                  (begin
                    (mset! ,tmp2 ,offset-vector-length ,tmp)
                    ,tmp2))))))]
        [(vector-length ,e)
          `(mref ,e ,offset-vector-length)]
        [(vector-ref ,e1 ,e2) 
          `(mref ,e1 (+ ,offset-vector-data ,e2))]
        [(void) $void]
        [(make-procedure ,code ,size) 
          (let ([tmp (unique-name 'tmp)])
            `(let ([,tmp (+ (alloc ,(+ disp-procedure-data size)) ,tag-procedure)])
               (begin
                 (mset! ,tmp ,offset-procedure-code ,code)
                 (mset! ,tmp ,offset-procedure-length ,size)
                 ,tmp)))]
        [(procedure-code ,proc) `(mref ,proc ,offset-procedure-code)]
        [(procedure-ref ,proc ,i) `(mref ,proc ,(+ offset-procedure-data i))]))

  (define (handle-pred-primitive expr)
    (match expr
      [(< ,e1 ,e2) expr]
      [(<= ,e1 ,e2) expr]
      [(= ,e1 ,e2) expr]
      [(>= ,e1 ,e2) expr]
      [(> ,e1 ,e2) expr]
      [(boolean? ,e1) `(= (logand ,e1 ,mask-boolean) ,tag-boolean)]
      [(fixnum? ,e1) `(= (logand ,e1 ,mask-fixnum) ,tag-fixnum)]
      [(null? ,e1) `(= ,e1 ,$nil)]
      [(pair? ,e1) `(= (logand ,e1 ,mask-pair) ,tag-pair)]
      [(vector? ,e1) `(= (logand ,e1 ,mask-vector) ,tag-vector)]
      [(procedure? ,e1) `(= (logand ,e1 ,mask-procedure) ,tag-procedure)]
      [(eq? ,e1 ,e2) `(= ,e1 ,e2)]))

  (define (handle-effect-primitive expr)
    (match expr
      [(set-car! ,e1 ,e2) `(mset! ,e1 ,offset-car ,e2)]
      [(set-cdr! ,e1 ,e2) `(mset! ,e1 ,offset-cdr ,e2)]
      [(vector-set! ,e1 ,e2 ,e3) 
        `(mset! ,e1 (+ ,offset-vector-data ,e2) ,e3)]
      [(procedure-set! ,e1 ,e2 ,e3)
        `(mset! ,e1 ,(+ offset-procedure-data e2) ,e3)]
      [(display ,e1) `(display ,e1)]
      [(newline) '(newline)]))

  (define (Value expr)
    (match expr
      [,x (guard (or (label? x) (uvar? x))) x]
      [(quote ,x) (guard (immediate? x)) (handle-immediate x)]
      [(if ,[Pred -> p] ,[v1] ,[v2]) `(if ,p ,v1 ,v2)]
      [(begin ,[Effect -> e*] ... ,[v]) `(begin ,e* ... ,v)]
      [(let ([,uv* ,[Value -> v*]] ...) ,[Value -> v])
        `(let ([,uv* ,v*] ...) ,v)]
      [(,prim ,[v*] ...) (guard (valop-prim? prim))
        (handle-value-primitive `(,prim ,v* ...))]
      [(,[v*] ...) `(,v* ...)]))
  (define (Pred expr)
    (match expr
      [(true) '(true)]
      [(false) '(false)]
      [(if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3)]
      [(begin ,[Effect -> e*] ... ,[p]) `(begin ,e* ... ,p)]
      [(let ([,uv* ,[Value -> v*]] ...) ,[p])
        `(let ([,uv* ,v*] ...) ,p)]
      [(,prim ,[Value -> v*] ...) (guard (predop-prim? prim))
        (handle-pred-primitive `(,prim ,v* ...))]))
  (define (Effect expr)
    (match expr
      [(nop) '(nop)]
      [(if ,[Pred -> p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2)]
      [(begin ,[e*] ...) `(begin ,e* ...)]
      [(let ([,uv* ,[Value -> v*]] ...) ,[e])
        `(let ([,uv* ,v*] ...) ,e)]
      [(,prim ,[Value -> v*] ...) (guard (effectop-prim? prim))
        (handle-effect-primitive `(,prim ,v* ...))]
      [(,[Value -> v*] ...) `(,v* ...)]))
  (match expr
    [(letrec ([,l* (lambda ,uv* ,[Value -> v*])] ...) ,[Value -> v])
      (fold-constants `(letrec ([,l* (lambda ,uv* ,v*)] ...) ,v))]))

;; +-------------------------------------------------------------------------+
;; | PASS    | fold-constants                                                |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | NOTE: THIS PASS IS CURRENTLY BEING RUN AS PART OF                       |
;; | SPECIFY-REPRESENTATION. IT NEEDS A WRAPPER WRITTEN FOR IT.              |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (fold-constants expr)                                         |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with all of the constants folded out.          |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass locals all computations that involve only integers and simply |
;; | performs the computations, replacing the expression with the resulting  |
;; | integer value.                                                          |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Tree Walk!                                                              |
;; +-------------------------------------------------------------------------+
;; | INPUT/OUTPUT GRAMMAR                                                    |
;; +-------------------------------------------------------------------------+
;; | Any, really.                                                            |
;; | Things that will be flattened: +, -, *, sra
;; +-------------------------------------------------------------------------+

(define-who (fold-constants expr)
  (match expr
    [,x (guard (atom? x)) x]
    [(+ ,[e1] ,[e2])
      (if (and (integer? e1) (integer? e2))
        (+ e1 e2)
        `(+ ,e1 ,e2))]
    [(- ,[e1] ,[e2])
      (if (and (integer? e1) (integer? e2))
        (- e1 e2)
        `(- ,e1 ,e2))]
    [(* ,[e1] ,[e2])
      (if (and (integer? e1) (integer? e2))
        (* e1 e2)
        `(* ,e1 ,e2))]
    [(sra ,[e1] ,[e2])
      (if (and (integer? e1) (integer? e2))
        (ash e1 e2)
        `(sra ,e1 ,e2))]
    [(,[a] . ,[d])
      `(,a . ,d)]))

)
