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

(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define (prime? n)
  (call/cc
   (lambda (return)
     (do ((i 2 (+ i 1)))
         ((> i (sqrt n)) #t)
       (if (= (modulo n i) 0) 
           (return #f))))))

(define gen-prime
  (lambda (hi)
    (let ((i (number-between 2 hi)))
      (assert (prime? i))
      i)))

(define-syntax bag-of
  (syntax-rules ()
    ((bag-of e)
     (let ((prev-amb-fail amb-fail)
           (results '()))
       (if (call/cc
            (lambda (k)                                                
              (set! amb-fail (lambda () (k #f)))                ;<-----+
              (let ((v e))             ;amb-fail will be modified by e |
                (set! results (cons v results))                       ;|
                (k #t))))                                             ;|
           (amb-fail))                 ;so this amb-fail may not be ---+
       (set! amb-fail prev-amb-fail)
       (reverse! results)))))
