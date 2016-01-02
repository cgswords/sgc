(library (compiler compile)
  (export p423-compile p423-step
          p423-tcompile p423-tstep)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (framework driver)
    (framework wrappers)
    (framework match)
    (framework helpers)
    ;; Load your passes from the file you wrote them in.
;;    (compiler verify-scheme)
    (compiler desugar-input)
    (compiler purify-assignments)
    (compiler lift-lambdas)
    (compiler convert-closures)
    (compiler convert-representation)
    (compiler flatten-let)
    (compiler verify-uil)
    (compiler cform-introduction)
    (compiler enforce-conventions)
    (compiler heap-allocator)
    (compiler stackframe-allocator)
    (compiler register-allocator)
    (compiler x86-generation))

  ;; Given a thunk that generates assembly code, this will compile the
  ;; resulting assembly code and output it to a file named t.s
  ;; (define runtime-files
  ;;   '("runtime.c" "printer.c" "cheney.c"))
  (define runtime-files
    '("runtime.c" "printer.c" "copygc.c" "metrics.c"))

  (define debug-flags
    (lambda ()
      (if #t
          '("-Wall" "-g")
          '())))

  (define asm-file 't.s)
  (define out-file 't)


(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system (format "cc -m64 ~{~s ~} -g -o ~a ~a ~{runtime/~s ~}" (debug-flags) out-file asm-file runtime-files)))
    (error 'assemble "assembly failed"))
  "./t")

;; (p423-compile (test-ref (valid-tests) #))

;; Defines the compiler
;; See the drivers.ss file for other options when defining a compiler

(define-compiler (p423-tcompile p423-tstep pass->wrapper)
  (parse-scheme)
  (convert-complex-datum)
  (trace uncover-assigned)
  (trace purify-letrec)
  (trace convert-assignments)
  (trace optimize-direct-call)
  (remove-anonymous-lambda)
  (trace sanitize-binding-forms)
  (uncover-free)
  (trace convert-closures)
  (optimize-known-call)
  (trace introduce-procedure-primitives)
  (lift-letrec)
  (trace normalize-context)
  (trace specify-representation)
;;(fold-constants)
  (uncover-locals)
  (remove-let)
  (trace verify-uil)
  (introduce-cform)
  (remove-complex-opera*)
  (flatten-set!)
  (expose-allocation-pointer)
  (trace impose-calling-conventions)
  (uncover-frame-conflict)
  (pre-assign-frame)
  (assign-new-frame)
 (iterate
    (finalize-frame-locations)
    (select-instructions)
    (uncover-register-conflict)
    (assign-registers)
    (break/when everybody-home?)
    (assign-frame) 
    )
  ;; (trace protect-ccall-lives)
  (discard-call-live)
  (trace finalize-locations)
  (expose-frame-var)
  (expose-memory-operands)
  (expose-basic-blocks)
  (optimize-jumps)
  (eliminate-cform)
  (trace flatten-program)
  (collect-strings)
  (generate-x86-64 assemble)  
  )

(define-compiler (p423-compile p423-step pass->wrapper)
  ;; desugar-input.ss 
  (parse-scheme)
  (convert-complex-datum)
  ;; purifty-assignments.ss
  (uncover-assigned)
  (purify-letrec)
  (convert-assignments)
  ;; lift-lambdas.ss
  (optimize-direct-call)
  (remove-anonymous-lambda)
  (sanitize-binding-forms)
  (uncover-free)
  ;; convert-closuers.ss
  (convert-closures)
  (optimize-known-call)
  (introduce-procedure-primitives)
  ;; convert-representation.ss
  (lift-letrec)
  (normalize-context)
  (specify-representation)
;;  (fold-constants)
  ;; flatten-let.ss
  (uncover-locals)
  (remove-let)
  ;; verify-uil.ss
  (verify-uil)
  ;; cform-introduction.ss
  (introduce-cform)
  ;; enforce-conventions.ss (a)
  (remove-complex-opera*)
  (flatten-set!)
  ;; heap-allocator.ss
  (expose-allocation-pointer)
  ;; enforce-conventions.ss (b)
  (impose-calling-conventions)
  ;; stackframe-allocator.ss
  (uncover-frame-conflict)
  (pre-assign-frame)
  (assign-new-frame)
  ;; register-allocator.ss
 (iterate
    (finalize-frame-locations)
    (select-instructions)
    (uncover-register-conflict)
    (assign-registers)
    (break/when everybody-home?)
    (assign-frame) 
    )
  ;;(protect-ccall-lives)
  (discard-call-live)
  (finalize-locations)
  ;; x86-generation.ss
  (expose-frame-var)
  (expose-memory-operands)
  (expose-basic-blocks)
  (optimize-jumps)
  (eliminate-cform)
  (flatten-program)
  (collect-strings)
  (generate-x86-64 assemble)  
  )
) ;; End library
