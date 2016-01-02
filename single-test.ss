(import (framework helpers))
(import (framework testing))
(import (framework test-suite))
(import (compiler compile))

(define (run-test n)
  (p423-compile (test-ref (valid-tests) n))
  (system "./t"))

(define (run-trace-test n)
  (p423-tcompile (test-ref (valid-tests) n))
  (system "./t"))

(define (step-test n)
  (p423-step (test-ref (valid-tests) n))
  (system "./t"))

;; (run-trace-test 162)

(define (test-print)
  (p423-tcompile
    '(let ((n 5))(display n)))
  (system "./t"))

(define (test-proc2 m)
  (p423-tcompile
   `(letrec ((div (lambda (n m k)
		    (if (< n m)
			(k 0)
			(div (- n m) m 
                         (lambda (q)
			   (k (+ q 1)))))))
	     (msort (lambda (ls k)
		      (if (or (null? ls) (null? (cdr ls))) 
			  (k ls) 
			  (part ls 
			   (lambda (p)
			     (msort (car p) 
			      (lambda (s1)
				(msort (cdr p) 
				 (lambda (s2)
				   (merge s1 s2 k))))))))))
	     (merge (lambda (l1 l2 k)
		      (if (null? l1)
			  (k l2)
			  (if (null? l2)
			      (k l1)
			      (if (< (car l1) (car l2))
				  (merge (cdr l1) l2
				   (lambda (ls)
				     (k (cons (car l1) ls))))
				  (merge (cdr l2) l1
				   (lambda (ls)
				     (k (cons (car l2) ls)))))))))
	     (part (lambda (ls k)
		       (length ls 
			(lambda (n) 
                         (div n 2 
                          (lambda (n)
			    (take n ls
                             (lambda (h)
                              (drop n ls 
			       (lambda (t)
				 (k (cons h t))))))))))))
	     (take (lambda (n ls k)
		     (if (= 0 n)
			 (k '())
			 (take (- n 1) (cdr ls)
                          (lambda (d)
			    (k (cons (car ls) d)))))))
	     (drop (lambda (n ls k)
		     (if (= 0 n)
			 (k ls)
			 (drop (- n 1) (cdr ls) k))))
	     (length (lambda (ls k)
		       (if (null? ls)
			   (k 0)
			   (length (cdr ls)
			    (lambda (n)
			      (k (+ n 1))))))))
      (msort ',(map (lambda (x) (random 10)) (iota m)) 
	     (lambda (x) x))))
  (printf "Done Compiling\n")
  (system "./t"))


(define (test-proc n)
  (p423-compile 
   `(letrec ((cons^ (lambda (a d)
		      (lambda (p)
			(p a d))))
	     (car^ (lambda (p)
		     (p (lambda (a d) a))))
	     (build (lambda (n ls)
		      (if (= n 0)
			  ls
			  (build (- n 1) (cons^ #t ls))))))
      (letrec ((iter (lambda (n l)
		       (if (= n 0)
			   (car^ l)
			   (iter (- n 1) (build 99999 (lambda (x) x)))))))
	(iter ,n (cons #t #t)))))
  (printf "Done Compiling\n")
  (system "./t"))

(define (test-pairs n)
  (p423-compile 
   `(letrec ((build (lambda (n ls)
		      (if (= n 0)
			  ls
			  (build (- n 1) (cons #t ls))))))
      (letrec ((iter (lambda (n l)
		       (if (= n 0)
			   (car l)
			   (iter (- n 1) (build 99999 '(#t)))))))
	(iter ,n '(#t)))))
  (printf "Done Compiling\n")
  (system "./t"))

(define (test-pair-2 m)
  (p423-tcompile
    `(letrec ((zero? (lambda (n) (if (= n 0) #t #f))))
       (letrec ((step (lambda (p) (let ((m (car p))) 
                                    (if (zero? 0) 
                                        #t 
                                        (step (cons (- m 1) '())))))))
         (step (cons ,m '())))))
  (system "./t"))


(define (test-pair m)
  (p423-tcompile
    `(let ((f ,m))
       (letrec ((step (lambda (p) (let ((m (car p))) 
                                    (if (= m 0) 
                                        #t 
                                        (step (cons (- m 1) '())))))))
         (step (cons ,m '())))))
  (system "./t"))

(define (test-vector m)
  (p423-tcompile
   `(let ((f ,m))
      (letrec ((step (lambda (p) (let ((f (vector-ref p 0))
				       (s (vector-ref p 1))
				       (t (vector-ref p 2))) 
				   (if (= f 0)
				       (if (= s 0)
					   (if (= t 0)
					       #t
					       (let ((v (make-vector t)))
						 (vector-set! v 0 f)
						 (vector-set! v 1 s)
						 (vector-set! v 2 (- t 1))
						 (step v)))
					   (let ((v (make-vector s)))
						 (vector-set! v 0 f)
						 (vector-set! v 1 (- s 1))
						 (vector-set! v 2 t)
						 (step v)))
				       (let ((v (make-vector f)))
						 (vector-set! v 0 (- f 1))
						 (vector-set! v 1 s)
						 (vector-set! v 2 t)
						 (step v)))))))
	(step '#(,(* m 1) ,(* m 5) ,(* m 10))))))
  (system "./t"))


(define (pair-gc) (test-pair 1000000))

(define (test-appendo m)
  (p423-tcompile
   `(letrec ((caar (lambda (p) (car (car p))))
             (vector (lambda (a) (let ((v (make-vector 1)))
                                   (vector-set! v 0 a)
                                   v)))
             (eqv? (lambda (a b) (= a b)))
             (zero? (lambda (x) (= x 0)))
             (assf  (lambda (f ls)
                      (if (null? ls)
                          #f
                          (if (f (caar ls))
                              ls
                              (assf f (cdr ls))))))
             (var (lambda  (c) (vector c)))
             (var? (lambda (x) (vector? x)))
             (var=? (lambda (x1 x2)
                      (= (vector-ref x1 0) (vector-ref x2 0))))
             (call/fresh (lambda (f)
                           (lambda (s/c)
                             (let ((c (cdr s/c)))
                               ((f (var c)) (cons (car s/c) (+ c 1)))))))
             (unit (lambda (s/c) (cons s/c mzero)))
             (mzero '())
             (choice (lambda (s/c f) (cons s/c f)))
             (== (lambda (u v)
                   (lambda (s/c)
                     (let ((s (unify u v (car s/c))))
                       (if s (unit (cons s (cdr s/c))) mzero)))))
             (unify (lambda (u v s)
                      (let ((u (walk u s)) (v (walk v s)))
                        (if (and (var? u) (var? v) (var=? u v)) s
                            (if (var? u) (ext-s u v s)
                                (if (var? v) (ext-s v u s)
                                    (if (and (pair? u) (pair? v))
                                        (let ((s (unify (car u) (car v) s)))
                                          (and s (unify (cdr u) (cdr v) s)))
                                        (if (eqv? u v) s #f))))))))
             (walk (lambda (u s)
                     (let ((pr (and (var? u) (assf (lambda (v) (var=? u v)) s))))
                       (if pr (walk (cdr pr) s) u))))
             (ext-s (lambda (x v s)
                      (if (occurs-check x v s) #f (cons (cons x  v) s))))
             (occurs-check (lambda (x v s)
                             (let ((v (walk v s)))
                               (if (var? v) (var=? v x)
                                   (if (pair? v) (or (occurs-check x (car v) s)
                                                     (occurs-check x (cdr v) s))
                                       #f)))))
             (disj (lambda (g1 g2)
                     (lambda (s/c)
                       (mplus (g1 s/c) (g2 s/c)))))
             (conj (lambda (g1 g2)
                     (lambda (s/c)
                       (bind (g1 s/c) g2))))
             (success (lambda (s/c)
                        (unit s/c)))
             (failure (lambda (s/c)
                        mzero))
             (mplus (lambda (stream1 stream2)
                      (if (null? stream1) stream2 
                          (if (procedure? stream1) (lambda () (mplus (stream1) stream2))
                              (choice (car stream1) (mplus (cdr stream1) stream2))))))
             (bind (lambda (stream g)
                     (if (null? stream) mzero
                         (if (procedure? stream) (lambda () (bind (stream) g))
                             (mplus (g (car stream)) (bind (cdr stream) g))))))
             (pull (lambda (stream)
                     (if (procedure? stream) (pull (stream)) stream)))
             (take (lambda (n stream)
                     (if (zero? n) '()
                         (let ((stream (pull stream)))
                           (if (null? stream) '() (cons (car stream) (take (- n 1) (cdr stream))))))))
             (empty-state '(() . 0))
             (call/goal (lambda (g) (g empty-state)))
             (appendo (lambda (l s out)
                        (disj
                         (conj (== '() l) (== s out))
                         (call/fresh
                          (lambda (a)
                            (call/fresh
                             (lambda (d)
                               (conj
                                (== (cons a d) l)
                                (call/fresh
                                 (lambda (res)
                                   (conj
                                    (== (cons a res) out)
                                    (lambda (s/c)
                                      (lambda ()
                                        ((appendo d s res) s/c))))))))))))))
             (call-appendo (lambda (n)
                             (take n (call/goal
                                      (call/fresh
                                       (lambda (q)
                                         (call/fresh
                                          (lambda (l)
                                            (call/fresh
                                             (lambda (s)
                                               (call/fresh
                                                (lambda (out)
                                                  (conj
                                                   (appendo l s out)
                                                   (== (cons l (cons s (cons out '()))) q)))))))))))))))
      (call-appendo ,m)))
  (system "./t"))





;; (run-trace-test 9)

