(define call/cc call-with-current-continuation) 
(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
	  (lambda ()
	    (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-syntax amb
  (syntax-rules ()
    ((amb alt ...)
     (let ((prev-amb-fail amb-fail))
       (call/cc
        (lambda (sk)

          (call/cc
           (lambda (fk)
             (set! amb-fail
                   (lambda ()
                     (set! amb-fail prev-amb-fail)
                     (fk 'fail)))
             (sk alt))) ...
             
             (prev-amb-fail)))))))

(define-syntax bag-of
  (syntax-rules ()
    ((bag-of e)
     (let ((prev-amb-fail amb-fail)
           (results '()))
       (cond ((call/cc
	       (lambda (k)                                                
		 (set! amb-fail (lambda () (k #f)))                ;<-----+
		 (let ((v e))             ;amb-fail will be modified by e |
		   (set! results (cons v results))                       ;|
		   (k #t))))                                             ;|
	      (amb-fail)))                 ;so this amb-fail may not be ---+
       (set! amb-fail prev-amb-fail)
       (reverse results))))) 

(define (number-between lo hi) 
  (let loop ((i lo))
    (if (> i hi) (amb)
	(amb i (loop (+ i 1))))))

(define (assert pred) 
  (cond ((not pred) (amb))))

(define (prime? n)
  (call/cc
   (lambda (return)
     (do ((i 2 (+ i 1)))
         ((> i (sqrt n)) #t)
       (cond ((= (modulo n i) 0) 
	      (return #f)))))))

(define gen-prime
  (lambda (hi)
    (let ((i (number-between 2 hi)))
      (assert (prime? i))
      i)))

;; (gen-prime 5) 
;; (bag-of (gen-prime 5)) 

;; 基于广度优先搜索的choose fail
;; [示例代码 22.14] choose 的 Scheme 版正确实现
(define *paths* ())
(define failsym '@)

;; 基于广度优先搜索的choose，这样可以避免choose死循环
(define (true-choose choices)
  (call-with-current-continuation
    (lambda (cc)
      (set! *paths* (append *paths*
;;当程序到达选择点的时候，与每一个选择相对应的续延都会被加入到用来保存路径的列表后面
          (map (lambda (choice)
              (lambda () (cc choice)))
            choices)))
      (fail))))

(define fail)

(call-with-current-continuation
  (lambda (cc)
    (set! fail
      (lambda ()
        (if (null? *paths*)
          (cc failsym)
          (let ((p1 (car *paths*)))
            (set! *paths* (cdr *paths*))
            (p1)))))))
