(library (compiler verify-uil)
  (export verify-uil)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | verify-uil                                                    |
;; | AUTHOR  | Andy Keep, Kent Dybvig, Cameron Swords                        |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (verify-uil expr)                                             |
;; +-------------------------------------------------------------------------+
;; | RETURNS | expr on success, or (void) + an error otherwise               |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | verify-iul accepts a single value and verifies that the value is a      |
;; | valid program for the universal intermediate language of the compiler.  |
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | Walks the grammar, making sure that everything adheres to it and        |
;; | producing an error otherwise.                                           |
;; +-------------------------------------------------------------------------+
;; | INPUT / OUTPUT GRAMMAR                                                  |
;; +-------------------------------------------------------------------------+
;; |  Program -> (letrec ([<label> (lambda (<uvar>*) <Body>)]*) <Body>)      |
;; |  Body    -> (locals (<uvar>*) <Tail>)                                   |
;; |  Tail    -> <Triv>                                                      |
;; |           | (alloc <Value>)                                             |
;; |           | (mref <Value> <Value>)                                      |
;; |           | (binop <Value> <Value>)                                     |
;; |           | (<Value> <Value>*)                                          |
;; |           | (if <Pred> <Tail> <Tail>)                                   |
;; |           | (begin <Effect>* <Tail>)                                    |
;; |  Pred    -> (true)                                                      |
;; |           | (false)                                                     |
;; |           | (<relop> <Value> <Value>)                                   |
;; |           | (if <Pred> <Pred> <Pred>)                                   |
;; |           | (begin <Effect>* <Pred>)                                    |
;; |  Effect  -> (nop)                                                       |
;; |           | (display <uvar|int>)                                        |
;; |           | (newline)                                                   |
;; |           | (set! <uvar> <Value>)                                       |
;; |           | (mset! <Value> <Value> <Value>)                             |
;; |           | (<Value> <Value>*)                                          |
;; |           | (if <Pred> <Effect> <Effect>)                               |
;; |           | (begin <Effect>* <Effect>)                                  |
;; |  Value   -> <Triv>                                                      |
;; |           | (alloc <Value>)                                             |
;; |           | (mref <Value> <Value>)                                      |
;; |           | (<binop> <Value> <Value>)                                   |
;; |           | (<Value> <Value>*)                                          |
;; |           | (if <Pred> <Value> <Value>)                                 |
;; |           | (begin <Effect>* <Value>)                                   |
;; |  Triv    -> <uvar> | <integer> | <label>                                |
;; +-------------------------------------------------------------------------+
;; | OTHER NOTES                                                             |
;; +-------------------------------------------------------------------------+
;; |  - uar is symbol.n, n >= 0                                              |
;; |  - binop is mref, +, -, *, logand, logor, or sra                        |
;; |  - relop is <, <=, =, >=, >                                             |
;; |  - label is symbol$n, n >= 0                                            |
;; |  - sra's second oeprand must be an exact integer k, 0 <= k <= 63        |
;; |  - each other integer must be a exact integer n, -2^63 <= n <= 2^63-1   |
;; +-------------------------------------------------------------------------+

(define-who verify-uil 
  (define binops '(+ - * logand logor sra mref))
  (define relops '(< > <= >= =))
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))
  (define Printable
    (lambda (label* uvar*)
      (lambda (t)
        (unless (or (uvar? t) (and (integer? t) (exact? t)))
          (error who "invalid Printable ~s" t))
        (when (and (integer? t) (exact? t))
          (unless (int64? t)
            (error who "integer out of 64-bit range ~s" t)))
        (when (uvar? t)
          (unless (memq t uvar*)
            (error who "reference to unbound uvar ~s" t)))
        (values))))
  (define Triv
    (lambda (label* uvar*)
      (lambda (t)
        (unless (or (label? t) (uvar? t) (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (and (integer? t) (exact? t))
          (unless (int64? t)
            (error who "integer out of 64-bit range ~s" t)))
        (when (uvar? t)
          (unless (memq t uvar*)
            (error who "reference to unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        (values))))
  (define Value
    (lambda (label* uvar*)
      (lambda (val)
        (match val
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(alloc ,[]) (values)]
          [(mref ,[] ,[]) (values)]
          [(sra ,[] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))
           (values)]
          [(,binop ,[] ,[])
           (guard (memq binop binops))
           (values)]
          [(,[] ,[] ...) (values)]
          [,[(Triv label* uvar*) ->] (values)]))))
  (define Effect
    (lambda (label* uvar*)
      (lambda (ef)
        (match ef
          [(nop) (values)]
          [(newline) (values)]
          [(display ,[(Printable label* uvar*) ->]) (values)]
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[] ... ,[]) (values)]
          [(set! ,var ,[(Value label* uvar*) ->])
           (unless (memq var uvar*)
             (error who "assignment to unbound var ~s" var))
           (values)]
          [(mset! ,[(Value label* uvar*) ->] 
                  ,[(Value label* uvar*) ->] 
                  ,[(Value label* uvar*) ->])
            (values)]
          [(display ,[(Value label* uvar*) ->]) (values)] ;; print one value
          [(newline) (values)]                            ;; print a newline
          [(,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->] ...) (values)]
          [,ef (error who "invalid Effect ~s" ef)]))))
  (define Pred
    (lambda (label* uvar*)
      (lambda (pr)
        (match pr
          [(true) (values)]
          [(false) (values)]
          [(if ,[] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(,relop ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (guard (memq relop relops))
           (values)]
          [,pr (error who "invalid Pred ~s" pr)]))))
  (define Tail
    (lambda (label* uvar*)
      (lambda (tail)
        (match tail
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(alloc ,[(Value label* uvar*) ->]) (values)]
          [(mref ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->]) (values)]
          [(sra ,[(Value label* uvar*) ->] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))
           (values)]
          [(,binop ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (guard (memq binop binops))
           (values)]
          [(,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->] ...) (values)]
          [,[(Triv label* uvar*) ->] (values)]))))
  (define Body
    (lambda (label* fml*)
      (lambda (x)
        (match x
          [(locals (,local* ...) ,tail)
           (let ([uvar* `(,fml* ... ,local* ...)])
             (verify-x-list uvar* uvar? 'uvar)
             ((Tail label* uvar*) tail)
             (values))]
          [,x (error who "invalid Body ~s" x)]))))
  (define Lambda
    (lambda (label*)
      (lambda (x)
        (match x
          [(lambda (,fml* ...) ,[(Body label* fml*) ->]) (values)]
          [,x (error who "invalid Lambda ~a" x)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* ,[(Lambda label*) ->]] ...) ,[(Body label* '()) ->])
       (verify-x-list label* label? 'label)]
      [,x (error who "invalid Program ~s" x)])
    x))

)
