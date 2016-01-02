(library (compiler helpers)
  (export
    comp
    symbol-append
    zip
    unzip
    build-bindings
    empty-env
    extend-env
    lookup-env
    primitive?
    binop?
    relop?
    immediate?
    jump?
    flatten
    remove-nulls
    make-nopless-begin
    debug-printer)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (framework wrappers)
    (framework match)
    (framework helpers))

;; +-------------------------------------------------------------------------+
;; | HELPERS                                                                 |
;; +-------------------------------------------------------------------------+
;; | AUTHOR      | Cameron Swords                                            |
;; +-------------------------------------------------------------------------+
;; | HELPER LIST |                                                           |
;; +-------------------------------------------------------------------------+
;; | zip         | Zips two lists.                                           |
;; | build-      | Used to build up lists of bindings for let and letrec.    |
;; |  bindings   | First arg is vars to bind, second is RHSs to bind.        |
;; | empty-env   | Returns an empty environment.                             |
;; | extend-env  | Extends then environment with a tag and some data.        |
;; | lookup-env  |  Looks up a tag in the environment                        |
;; | primitive?  | Determines if something is a primitive.                   |
;; | binop?      | Determines if something is a binary operator.             |
;; | relop?      | Determines if something is a relational operator.         |
;; | immediate?  | Determines if something is an immediate.                  |
;; | jump?       | Determines if something is a jump of the form (loc).      |
;; | flatten     | Flattens nested lists into a flat list.                   |
;; | remove-nulls| Walks a list, removing any '()s.                          |
;; | make-nopless| Like make-begin, but removes all nops. Returns (nop) if   | 
;; |   -begin    | it shouldn't make the begin.                              |
;; |debug-printer| A nice printer for debugging purposes.                    |
;; +-------------------------------------------------------------------------+

(define symbol-append
  (lambda args
    (string->symbol (apply string-append (map symbol->string args)))))

;; +-------------------------------------------------------------------------+
;; | HELPER  | zip                                                           |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (zip a b)                                                     |
;; +-------------------------------------------------------------------------+
;; | Zips two list into an association list of the form ((a . b) ...)        |
;; +-------------------------------------------------------------------------+
(define zip
  (lambda (l1 l2)
    (cond
      [(null? l1) '()]
      [else `((,(car l1) . ,(car l2)) . ,(zip (cdr l1) (cdr l2)))])))

(define unzip
  (lambda (alist)
    (cond
      [(null? alist) (values '() '())]
      [else (let-values (((ls1 ls2) (unzip (cdr alist))))
              (values (cons (caar alist) ls1) (cons (cadar alist) ls2)))])))

;; +-------------------------------------------------------------------------+
;; | HELPER  | build-bindings                                                |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (build-bindings a b)                                          |
;; +-------------------------------------------------------------------------+
;; | Builds a list of bindings of the form ((a (b)) ...)                     |
;; +-------------------------------------------------------------------------+
(define build-bindings
  (lambda (l1 l2)
    (cond
      [(null? l1) '()]
      [else `((,(car l1) . (,(car l2))) . 
              ,(build-bindings (cdr l1) (cdr l2)))])))

;; +-------------------------------------------------------------------------+
;; | HELPER  | empty-env                                                     |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (empty-env)                                                   |
;; +-------------------------------------------------------------------------+
;; | Returns an empty environment for usage.                                 |
;; +-------------------------------------------------------------------------+
(define empty-env (lambda () '()))

;; +-------------------------------------------------------------------------+
;; | HELPER  | extend-env                                                    |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (extend-env tag data old-env)                                 |
;; +-------------------------------------------------------------------------+
;; | Extends the environment passed in by associating the tag with the data. |
;; +-------------------------------------------------------------------------+
(define extend-env 
  (lambda (tag data env)
    `((,tag . ,data) . ,env)))

;; +-------------------------------------------------------------------------+
;; | HELPER  | lookup-env                                                    |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (lookup-env tag env)                                          |
;; +-------------------------------------------------------------------------+
;; | Looks through the environment, returning the data associated with the   |
;; | tag passed in. If it is not in the environment, it returns #f.          |
;; +-------------------------------------------------------------------------+
(define lookup-env
  (lambda (tag env)
    (let ([res (assq tag env)])
         (if res (cdr res) #f))))

;; +-------------------------------------------------------------------------+
;; | HELPER  | primitive?                                                    |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (primitive? x)                                                |
;; +-------------------------------------------------------------------------+
;; | Returns a partial list if x is a primitive operator or #f if x is not.  |
;; +-------------------------------------------------------------------------+
(define primitive?
  (lambda (prim)
    (memq prim 
      '(+ - * car cdr cons make-vector vector-length vector-ref void 
        < <= = >= > boolean? eq? fixnum? null? pair? vector? procedure?
        set-car! set-cdr! vector-set!
        make-procedure procedure-ref procedure-code procedure-set!
        display newline))))

;; +-------------------------------------------------------------------------+
;; | HELPER  | binop?                                                        |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (binop? x)                                                    |
;; +-------------------------------------------------------------------------+
;; | Returns a partial list if x is a binary operator or #f if x is not.     |
;; | Operators: + - logand logor                                             |
;; +-------------------------------------------------------------------------+

(define (binop? x)
  (memq x '(+ - logand logor mref)))

;; +-------------------------------------------------------------------------+
;; | HELPER  | relop?                                                        |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (relop? x)                                                    |
;; +-------------------------------------------------------------------------+
;; | Returns a partial list if x is a relational operator or #f if x is not. |
;; | Operators: < <= = >= >                                                  |
;; +-------------------------------------------------------------------------+
(define (relop? x)
  (memq x '(< <= = >= >)))

;; +-------------------------------------------------------------------------+
;; | HELPER  | immediate?                                                    |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (immediate? x)                                                |
;; +-------------------------------------------------------------------------+
;; | Returns a partial list if x is an immediate and #f if it is not.        |
;; | Immediates: fixnums () #t #f                                            |
;; +-------------------------------------------------------------------------+
(define (immediate? expr)
      (or (integer? expr) (memq expr '(#t #f ()))))

;; +-------------------------------------------------------------------------+
;; | HELPER  | jump?                                                         |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (jump? x)                                                     |
;; +-------------------------------------------------------------------------+
;; | Returns #t if x is a valid jump of the form ([jump-loc]) and #f if not. |
;; +-------------------------------------------------------------------------+
(define (jump? x)
  (match x
    [(,x) (guard (or (label? x) (register? x) (frame-var? x) (uvar? x))) #t]
    [,x #f]))

;; +-------------------------------------------------------------------------+
;; | HELPER  | flatten                                                       |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (flatten a b)                                                 |
;; +-------------------------------------------------------------------------+
;; | Flattens out a list, removing any nesting in it.                        |
;; +-------------------------------------------------------------------------+
(define flatten
 (lambda (x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x)) (flatten (cdr x)))))))

;; +-------------------------------------------------------------------------+
;; | HELPER  | remove-nulls                                                  |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (remove-nulls ls)                                             |
;; +-------------------------------------------------------------------------+
;; | Takes a deep list and removes any elements of the form () from it.      |
;; +-------------------------------------------------------------------------+
(define remove-nulls
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (car ls)) (remove-nulls (cdr ls))]
      [else (cons (car ls) (remove-nulls (cdr ls)))])))


;; +-------------------------------------------------------------------------+
;; | HELPER  | make-nopless-begin                                            |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (make-nopless-begin ls)                                       |
;; +-------------------------------------------------------------------------+
;; | Takes a list of expressions to make a begin out of and calls make-begin |
;; | after ensuring that all of the (nop) commands are removed.              | 
;; +-------------------------------------------------------------------------+
(define (make-nopless-begin x*)
  (let ([x* (remove '(nop) x*)])
    (if (null? x*)
      '(nop)
      (make-begin x*))))

;; +-------------------------------------------------------------------------+
;; | HELPER  | debug-printer                                                 |
;; +-------------------------------------------------------------------------+
;; | USAGE   | (debug-printer pass expr)                                     |
;; +-------------------------------------------------------------------------+
;; | Takes a pass name and some expression and prints them both, returning   |
;; | the second argument.                                                    |
;; +-------------------------------------------------------------------------+

(define (debug-printer pass expr)
  (printf "=========================================================\n~a\n=========================================================\n" pass )
  (pretty-print expr)
  (newline)
  expr)

(define comp
  (case-lambda 
    [(f) (lambda (x) (f x))]
    [(f g) (lambda (x) (g (f x)))]
    [(f g h) (lambda (x) (h (g (f x))))]))

)
