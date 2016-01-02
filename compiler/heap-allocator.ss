(library (compiler heap-allocator)
  (export expose-allocation-pointer)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;; +-------------------------------------------------------------------------+
;; | PASS    | expose-allocation-pointer                                     |
;; | AUTHOR  | Cameron Swords                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (expose-allocation-pointer expr)                              |
;; +-------------------------------------------------------------------------+
;; | RETURNS | The expression with the allocation form replaced by the       |
;; |         | correct register operations.                                  |
;; +-------------------------------------------------------------------------+
;; | DESCRIPTION                                                             |
;; +-------------------------------------------------------------------------+
;; | This pass removes the allocation form. Any allocation gets the current  |
;; | value of the allocation-pointer-register and then said register gets    |
;; | incremented by the size of the allocation.                              | 
;; +-------------------------------------------------------------------------+
;; | IMPLEMENTATION                                                          |
;; +-------------------------------------------------------------------------+
;; | It uses a tree walk. The walk only considers one case:                  |
;; |   (set! uvar (alloc size:Triv))                                         |
;; | This case is expanded to allocate the required memory by replace this   |
;; | form with a form that provides the correct semantics:                   |
;; |   (begin                                                                |
;; |     (set! uvar alloc-ptr-reg)                                           |
;; |     (set! alloc-ptr-reg (+ alloc-ptr-reg size))                         |
;; +-------------------------------------------------------------------------+
;; | INPUT GRAMMAR                                                           |
;; +-------------------------------------------------------------------------+
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) (new-frames (Frame*) Tail))                  |
;; | Tail    -> (Triv Loc*)                                                  |
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
;; |          | (set! uvar (alloc Triv))                                     |
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
;; | Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)               |
;; | Body    -> (locals (uvar*) (new-frames (Frame*) Tail))                  |
;; | Tail    -> (Triv Loc*)                                                  |
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
;; |          | (mset! Triv Triv Triv)                                       |
;; |          | (return-point label Tail)                                    |
;; |          | (if Pred Effect Effect)                                      |
;; |          | (begin Effect* Effect)                                       |
;; | Loc     -> reg | fvar                                                   |
;; | Triv    -> uvar | int | label                                           |
;; | Frame   -> (uvar*)                                                      |
;; +-------------------------------------------------------------------------+

(define-who (expose-allocation-pointer expr)
  (define ar allocation-pointer-register)
  (define fp frame-pointer-register)
  (define (Body expr)
    (match expr
      [,x (guard (atom? x)) x]
      [(set! ,x (alloc ,size))
        `(begin  (ccall ,ar "collect" ,fp ,ar ,size) (set! ,x ,ar) (set! ,ar (+ ,ar ,size)))]
      [(,[a] . ,[d]) `(,a . ,d)]))
  (match expr
    [(letrec ([,l (lambda ,arg* ,[Body -> b*])] ...) ,[Body -> b])
      `(letrec ([,l (lambda ,arg* ,b*)] ...) ,b)]))

)
