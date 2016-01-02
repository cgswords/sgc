(library (framework wrappers aux)
  (export
    env
    alloc
    handle-overflow
    set!
    ccall
    call
    push
    pop
    ret
    display
    newline
    string
    text
    rewrite-opnds
    code
    jump
    (rename (p423-letrec letrec))
    locals
    locate
    ulocals
    spills
    register-conflict
    true
    false
    nop
    frame-conflict
    compute-frame-size
    call-live
    return-point-complex
    return-point-simple
    new-frames
    (rename (p423-* *))
    (rename (p423-+ +))
    (rename (p423-- -))
    free
    bind-free
    fill-closure!
    closures
    cookie
    procedure
    procedure?
    make-procedure
    procedure-code
    procedure-env
    procedure-set!
    procedure-ref
    assigned)
  (import
    (except (chezscheme) set! procedure? display newline string)
    (framework match)
    (framework helpers))

(define env
  (environment
    '(chezscheme)
    '(framework helpers)
    '(framework helpers frame-variables)))

(define-syntax wrap
  (syntax-rules ()
    ((_ (define-syntax name body))
     (define name `(define-syntax name body)))
    ((_ (define name body))
     (define name `(define name body)))))

(wrap
  (define alloc
    (lambda (nbytes)
      (unless (fxzero? (fxremainder nbytes word-size))
        (error 'alloc "~s is not a multiple of word size" nbytes))
      (let ([addr ,allocation-pointer-register])
        (set! ,allocation-pointer-register (+ addr nbytes))
        (check-heap-overflow ,allocation-pointer-register)
        addr))))

(define int64-in-range?
  (let ()
    (import scheme)
    (lambda (x)
      (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))

(define handle-overflow
  (let ()
    (import scheme)
    (lambda (x)
      (cond
        [(not (number? x)) x]
        [(int64-in-range? x) x]
        [(not (= x (logand 18446744073709551615 x)))
         (handle-overflow (logand 18446744073709551615 x))]
        [(< x 0) (handle-overflow (+ x (expt 2 64)))]
        [else (handle-overflow (- x (expt 2 64)))]))))

(define rewrite-opnds
  (lambda (x)
    (match x
      [,r (guard (disp-opnd? r))
        `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
      [,r (guard (index-opnd? r))
        `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
      [(set! ,r ,[expr]) (guard (disp-opnd? r))
       `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
      [(set! ,r ,[expr]) (guard (index-opnd? r))
       `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
      [(,[expr] ...) expr]
      [,x x])))

(define compute-frame-size
  (lambda (x)
    (match x
      [(,[fs*] ...) (apply max 0 fs*)]
      [,x (if (frame-var? x) (+ (frame-var->index x) 1) 0)])))

(wrap
  (define-syntax set!
    (let ()
      (import scheme)
      (syntax-rules (,frame-pointer-register)
        [(_ ,frame-pointer-register (op xxx n))
         (begin
           (fp-offset (op (fp-offset) n))
           (set! ,frame-pointer-register (op xxx n)))]
        [(_ x expr)
         (set! x (handle-overflow expr))]))))

(define-syntax code
  (lambda (x)
    (define build
      (lambda (body)
        (syntax-case body ()
          [() #'(())]
          [(label expr ...)
           (identifier? #'label)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'(((bounce label))
                (define label
                  (lambda ()
                    (bounce (lambda () expr ...))))
                defn ...))]
          [(expr1 expr ...)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'((expr1 expr ...) defn ...))])))
    (syntax-case x ()
      [(k expr ...)
       (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
         #'((call/cc
              (lambda (bounce)
                defn ...
                expr ...))))])))

(define-syntax jump
  (syntax-rules ()
    [(_ target) (target)]))

(define-syntax locals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax ulocals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax spills
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax frame-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax register-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax locate
  (let ()
    (import scheme)
    (syntax-rules ()
      [(_ ([x* loc*] ...) body)
       (let-syntax ([x* (identifier-syntax 
                          (id loc*) 
                          ((set! id e) 
                           (set! loc* (handle-overflow e))))] ...)
         body)])))

(define-syntax p423-letrec
  (let ()
    (import scheme)
    (syntax-rules (lambda)
      [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
       (letrec ([lab (lambda ignore (parameterize ([fp-offset 0]) lambda-body))] ...)
         (parameterize ([fp-offset 0]) letrec-body))])))

(define return-point-complex
  `(define-syntax return-point
     (lambda (x)
       (import scheme)
       (syntax-case x ()
         [(id rplab expr)
          #'(let ([top (fxsll frame-size word-shift)]
                  [rplab (lambda args (void))])
              (parameterize ([fp-offset (+ (fp-offset) top)])
                (set! ,frame-pointer-register
                  (+ ,frame-pointer-register top))
                expr
                (set! ,frame-pointer-register
                  (- ,frame-pointer-register top))))]))))

(define return-point-simple
  `(define-syntax return-point
     (syntax-rules ()
       ((_ lab expr)
        (let ([lab (lambda args (void))]) expr)))))

(wrap
  (define-syntax new-frames
    (lambda (x)
      (import scheme)
      (syntax-case x (return-point)
        [(id ((nfv ...) ...) expr)
         (with-syntax ([((i ...) ...) (map enumerate #'((nfv ...) ...))])
           #'(let ([top (fxsll frame-size word-shift)])
               (define-syntax nfv
                 (identifier-syntax
                   [id (mref (- ,frame-pointer-register (fp-offset))
                         (fxsll (+ i frame-size) word-shift))]
                   [(set! id e) 
                    (mset! (- ,frame-pointer-register (fp-offset))
                      (fxsll (+ i frame-size) word-shift)
                      e)]))
               ...
               ...
               expr))]))))

(define-syntax call-live
  (syntax-rules ()
    [(_ (x* ...) body) body]))

(define-who p423-*
  (lambda (x y)
    (import scheme)
    (let ([ans (* x y)])
      (unless (fixnum-range? ans)
        (errorf who "result ~s is outside of fixnum range" ans))
      ans)))

(define-who p423-+
  (lambda (x y)
    (import scheme)
    (let ([ans (+ x y)])
      (unless (fixnum-range? ans)
        (errorf who "result ~s is outside of fixnum range" ans))
      ans)))

(define-who p423--
  (lambda (x y)
    (import scheme)
    (let ([ans (- x y)])
      (unless (fixnum-range? ans)
        (errorf who "result ~s is outside of fixnum range" ans))
      ans)))

(define-syntax free
    (syntax-rules ()
      [(_ (var ...) expr) expr]))

(define-syntax bind-free
  (lambda (x)
    (syntax-case x ()
      [(_ (cp fv ...) body)
       (with-syntax ([(i ...) (enumerate #'(fv ...))])
         #'(let ()
             (define-syntax fv
               (identifier-syntax
                 (vector-ref (cp cookie) i)))
             ...
             body))])))

(define fill-closure!
  (lambda (cp . free)
    (let ([env (cp cookie)])
      (for-each
        (lambda (i x) (vector-set! env i x))
        (enumerate free)
        free))))

(define-syntax closures
  (syntax-rules ()
    [(_ ([name code free ...] ...) body)
     (letrec ([name (let ([env (make-vector (length '(free ...)))])
                           (lambda args
                             (if (and (= (length args) 1)
                                      (eq? (car args) cookie))
                                 env
                                 (apply code args))))]
                   ...)
       (fill-closure! name free ...)
       ...
       body)]))

(define-syntax well-known
  (syntax-rules ()
    [(_ (wk ...) body) body]))

(define-record procedure ((immutable code) (immutable env)) ()
  ([constructor $make-procedure]))

(define make-procedure
    (lambda (code i)
      ($make-procedure code (make-vector i))))

(define procedure-ref
    (lambda (cp i)
      (vector-ref (procedure-env cp) i)))

(define procedure-set!
    (lambda (cp i v)
      (vector-set! (procedure-env cp) i v)))

(define-syntax assigned
    (syntax-rules ()
      [(_ (var ...) expr) expr]))

(define (true) #t)

(define (false) #f)

(define (ccall . args) (void))
(define (call callee) (void))
(define (push val) (void))
(define (pop reg) (void))
(define (ret) (void))

(define (string x) (void))

(define (text) (void))

(define (display x) (void))

(define (newline) (void))

(define (nop) (void))

(define cookie (cons "snicker" "doodle"))
)

(library (framework wrappers)
  (export
    pass->wrapper
    source/wrapper
    parse-scheme/wrapper
    convert-complex-datum/wrapper
    uncover-assigned/wrapper
    purify-letrec/wrapper
    convert-assignments/wrapper
    optimize-direct-call/wrapper
    remove-anonymous-lambda/wrapper
    optimize-self-reference/wrapper
    optimize-free/wrapper
    uncover-well-known/wrapper
    sanitize-binding-forms/wrapper
    uncover-free/wrapper
    convert-closures/wrapper
    introduce-procedure-primitives/wrapper
    lift-letrec/wrapper
    normalize-context/wrapper
    optimize-jumps/wrapper
    specify-representation/wrapper
    uncover-locals/wrapper
    remove-let/wrapper
    verify-uil/wrapper
    introduce-cform/wrapper
    remove-complex-opera*/wrapper
    flatten-set!/wrapper
    impose-calling-conventions/wrapper
    expose-allocation-pointer/wrapper
    uncover-frame-conflict/wrapper
    pre-assign-frame/wrapper
    assign-new-frame/wrapper
    finalize-frame-locations/wrapper
    select-instructions/wrapper
    uncover-register-conflict/wrapper
    assign-registers/wrapper
    assign-frame/wrapper
    protect-ccall-lives/wrapper
    discard-call-live/wrapper
    finalize-locations/wrapper
    expose-frame-var/wrapper
    expose-memory-operands/wrapper
    expose-basic-blocks/wrapper
    eliminate-cform/wrapper
    flatten-program/wrapper
    collect-strings/wrapper
    generate-x86-64/wrapper)
  (import
    (except (chezscheme) set!)
    (framework match)
    (framework helpers)
    (framework driver)
    (only (framework wrappers aux)
      env rewrite-opnds compute-frame-size
      return-point-complex return-point-simple
      new-frames set! alloc))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((parse-scheme) parse-scheme/wrapper)
      ((convert-complex-datum) convert-complex-datum/wrapper)
      ((uncover-assigned) uncover-assigned/wrapper)
      ((purify-letrec) purify-letrec/wrapper)
      ((convert-assignments) convert-assignments/wrapper)
      ((optimize-direct-call) optimize-direct-call/wrapper)
      ((remove-anonymous-lambda) remove-anonymous-lambda/wrapper)
      ((optimize-self-reference) optimize-self-reference/wrapper)
      ((optimize-free) optimize-free/wrapper)
      ((uncover-well-known) uncover-well-known/wrapper) 
      ((sanitize-binding-forms) sanitize-binding-forms/wrapper)
      ((uncover-free) uncover-free/wrapper)
      ((convert-closures) convert-closures/wrapper)
      ((optimize-known-call) optimize-known-call/wrapper)
      ((introduce-procedure-primitives) introduce-procedure-primitives/wrapper)
      ((lift-letrec) lift-letrec/wrapper)
      ((normalize-context) normalize-context/wrapper)
      ((specify-representation) specify-representation/wrapper)
      ((uncover-locals) uncover-locals/wrapper)
      ((remove-let) remove-let/wrapper)
      ((verify-uil) verify-uil/wrapper)
      ((introduce-cform) introduce-cform/wrapper)
      ((remove-complex-opera*) remove-complex-opera*/wrapper)
      ((flatten-set!) flatten-set!/wrapper)
      ((impose-calling-conventions) impose-calling-conventions/wrapper)
      ((expose-allocation-pointer) expose-allocation-pointer/wrapper)
      ((uncover-frame-conflict) uncover-frame-conflict/wrapper)
      ((pre-assign-frame) pre-assign-frame/wrapper)
      ((assign-new-frame) assign-new-frame/wrapper)
      ((finalize-frame-locations) finalize-frame-locations/wrapper)
      ((select-instructions) select-instructions/wrapper)
      ((uncover-register-conflict) uncover-register-conflict/wrapper)
      ((assign-registers) assign-registers/wrapper)
      ((assign-frame) assign-frame/wrapper)
      ((protect-ccall-lives) protect-ccall-lives/wrapper)
      ((discard-call-live) discard-call-live/wrapper)
      ((finalize-locations) finalize-locations/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((expose-memory-operands) expose-memory-operands/wrapper)
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((optimize-jumps) optimize-jumps/wrapper)
      ((eliminate-cform) eliminate-cform/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((collect-strings) collect-strings/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

;;-----------------------------------
;; source/wrapper
;; verify-scheme/wrapper
;; optimize-direct-call/wrapper
;; remove-anonymous-lambda/wrapper
;; convert-complex-datum/wrapper
;; convert-assignments/wrapper
;; sanitize-binding-forms/wrapper
;;-----------------------------------
(define-language-wrapper
  (source/wrapper parse-scheme/wrapper
   optimize-direct-call/wrapper
   remove-anonymous-lambda/wrapper
   convert-complex-datum/wrapper
   convert-assignments/wrapper
   sanitize-binding-forms/wrapper)
  (x)
  (environment env)
  (import
    (only (framework wrappers aux) newline display * + -)
    (except (chezscheme) newline display * + -))
  (reset-machine-state!)
  ,x)

;;-----------------------------------
;; uncover-assigned/wrapper
;; purify-letrec/wrapper
;;-----------------------------------
(define-language-wrapper
  (uncover-assigned/wrapper
   purify-letrec/wrapper)
  (x)
  (environment env)
  (import
    (only (framework wrappers aux) newline display * + - assigned)
    (except (chezscheme) newline display * + -))
  ,x)


;;-----------------------------------
;; uncover-free/wrapper/wrapper
;;-----------------------------------
(define-language-wrapper uncover-free/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux) newline display * + - free)
    (except (chezscheme) newline display * + -))  
  ,x)

;;-----------------------------------
;; convert-closures/wrapper
;; optimize-known-call/wrapper
;; analyze-closure-size/wrapper
;; optimize-free/wrapper
;; optimize-self-reference/wrapper
;;-----------------------------------
(define-language-wrapper
  (convert-closures/wrapper optimize-known-call/wrapper
   analyze-closure-size/wrapper optimize-free/wrapper
   optimize-self-reference/wrapper) 
  (x)
  (environment env)
  (import
    (only (framework wrappers aux) newline display
      * + - cookie bind-free closures)
    (except (chezscheme) newline display * + -))
  ,x)



;;-----------------------------------
;; uncover-well-known
;;-----------------------------------
(define-language-wrapper
  (uncover-well-known/wrapper) 
  (x)
  (environment env)
  (import
    (only (framework wrappers aux) newline display
      * + - cookie bind-free closures well-known)
    (except (chezscheme) newline display * + -))
  ,x)


;;----------------------------------------
;; introduce-procedure-primitives/wrapper
;; lift-letrec/wrapper
;;----------------------------------------
(define-language-wrapper
  (introduce-procedure-primitives/wrapper lift-letrec/wrapper)
  (x) 
  (environment env)
  (import
    (only (framework wrappers aux) newline display
      * + - procedure make-procedure procedure-ref
      procedure-set! procedure-code procedure?)
    (except (chezscheme) newline display * + - procedure?))
  ,x)

;;-----------------------------------
;; normalize-context/wrapper
;;-----------------------------------
(define-language-wrapper
  normalize-context/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux) newline display
      true false nop * + -
      procedure make-procedure procedure-ref
      procedure-set! procedure-code procedure?)
    (except (chezscheme) newline display * + - procedure?))
  ,x)

;;-----------------------------------
;; specify-representation/wrapper
;;-----------------------------------
(define-language-wrapper
  specify-representation/wrapper
  (x)
  (environment env)
  ,alloc
  (import
    (only (framework wrappers aux) newline display
      handle-overflow true false nop))
  (ptr->datum ,x))

;;-----------------------------------
;; uncover-locals/wrapper
;;-----------------------------------
(define-language-wrapper
  uncover-locals/wrapper
  (x)
  (environment env)
  ,alloc
  (import
    (only (framework wrappers aux) newline display
      handle-overflow locals true false nop)
    (except (chezscheme) newline display set!))
  (ptr->datum ,x))

;;-----------------------------------
;; verify-uil/wrapper
;;-----------------------------------
(define-language-wrapper
  verify-uil/wrapper
  (x)
  (environment env)
  ,set! ,alloc
  (import
    (only (framework wrappers aux) newline display
      handle-overflow locals true false nop)
    (except (chezscheme) newline display set! lambda))
  (ptr->datum ,x))

;;-----------------------------------
;; introduce-cform/wrapper
;; remove-let/wrapper
;; remove-complex-opera*/wrapper
;; flatten-set!/wrapper
;;-----------------------------------
(define-language-wrapper
  (introduce-cform/wrapper remove-let/wrapper
   remove-complex-opera*/wrapper flatten-set!/wrapper)
  (x)
  (environment env)
  ,set! ,alloc
  (import
    (only (framework wrappers aux) 
      handle-overflow locals true false nop ccall)
    (except (chezscheme) set! lambda))
  (ptr->datum ,x))

;;-----------------------------------
;; impose-calling-conventions/wrapper
;;-----------------------------------
(define-language-wrapper impose-calling-conventions/wrapper
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,alloc
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locals true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; expose-allocation-pointer/wrapper
;;-----------------------------------
(define-language-wrapper expose-allocation-pointer/wrapper
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locals true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; uncover-frame-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-frame-conflict/wrapper
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locals spills call-live
      frame-conflict true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;----------------------------------
;; pre-assign-frame
;;----------------------------------
(define-language-wrapper pre-assign-frame/wrapper (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locals locate call-live 
      frame-conflict true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;----------------------------------
;; assign-new-frame
;;----------------------------------
(define-language-wrapper assign-new-frame/wrapper (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locals ulocals spills locate
      frame-conflict true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,x))
  (ptr->datum ,return-value-register))


;;-----------------------------------
;; finalize-frame-locations/wrapper
;; select-instructions/wrapper
;; assign-frame/wrapper
;;-----------------------------------
(define-language-wrapper
  (finalize-frame-locations/wrapper
   select-instructions/wrapper
   assign-frame/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locate
      locals ulocals frame-conflict
      true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; uncover-register-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-register-conflict/wrapper (x) 
  (environment env)
  ,set!
  ,return-point-simple
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locate locals ulocals frame-conflict
      register-conflict true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; assign-registers/wrapper
;;-----------------------------------
(define-language-wrapper assign-registers/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locate locals ulocals spills
      frame-conflict true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; protect-ccall-lives/wrapper
;;-----------------------------------
(define-language-wrapper protect-ccall-lives/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locate true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))



;;-----------------------------------
;; discard-call-live/wrapper
;;-----------------------------------
(define-language-wrapper discard-call-live/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec locate true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; finalize-locations/wrapper
;;-----------------------------------
(define-language-wrapper finalize-locations/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow letrec true false nop ccall)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; expose-frame-var/wrapper
;; expose-memory-operands/wrapper
;;-----------------------------------
(define-language-wrapper
  (expose-frame-var/wrapper expose-memory-operands/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow true false nop ccall)
    (except (chezscheme) set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; expose-basic-blocks/wrapper
;;-----------------------------------
(define-language-wrapper
  (expose-basic-blocks/wrapper optimize-jumps/wrapper)
  (x)
  (environment env)
  ,set!
  (import
    (only (framework wrappers aux) handle-overflow ccall)
    (except (chezscheme) set!))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  (ptr->datum ,return-value-register))


;;-----------------------------------
;; eliminate-cform/wrapper
;;-----------------------------------

(define-language-wrapper
   eliminate-cform/wrapper
   (x)
   (environment env)
   ,set!
   (import
    (only (framework wrappers aux) handle-overflow push pop ret call)
    (except (chezscheme) set!))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; flatten-program/wrapper
;;-----------------------------------
(define-language-wrapper flatten-program/wrapper (x)
  (environment env)
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow code jump push pop ret call)
    (except (chezscheme) set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; flatten-program/wrapper
;;-----------------------------------
(define-language-wrapper collect-strings/wrapper (x)
  (environment env)
  ,set!
  (import
    (only (framework wrappers aux)
      handle-overflow code jump push pop ret call string text)
    (except (chezscheme) set! string))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  (ptr->datum ,return-value-register))


;;-----------------------------------
;; generate-x86/wrapper
;;-----------------------------------
(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (get-line in)))

)
