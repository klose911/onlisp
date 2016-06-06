;; 15.1 函数的构造
;; 若f和g均为函数，则f○g(x) = f(g(x))
;; 用函数实现
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))

(compose #'list #'1+)
;; => #<FUNCTION :LAMBDA (&REST ARGS)
;;  (REDUCE #'FUNCALL FNS :FROM-END T :INITIAL-VALUE (APPLY FN1 ARGS))>
(car (last '(#'list #'1+)))  ;; => #'1+
(butlast '(#'list #'1+)) ;; (#'LIST)

#'(lambda (&rest args)
    (reduce #'funcall '(#'list #'1+) 
	    :from-end t
	    :initial-value (apply #'1+ args))) 
;; => #<FUNCTION :LAMBDA (&REST ARGS)
;;   (REDUCE #'FUNCALL '(#'LIST #'1+) :FROM-END T :INITIAL-VALUE
;;    (APPLY #'1+ ARGS))>

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4)) ;; => 4

;; 用宏来简化
(defmacro fn (expr)
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
	  (build-compose (cdr expr))
	  (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
			  `(,(rbuild f) ,g))
		      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		      (if fns
			  `(,(rbuild (car fns))
			     ,(rec (cdr fns)))
			  g)))
		(rec fns)))))

(fn (and integerp oddp)) ;; => #<FUNCTION :LAMBDA (#:G3399) (AND (INTEGERP #:G3399) (ODDP #:G3399))> 
(rbuild '(and integerp oddp)) ;; => (LAMBDA (#:G3402) (AND (INTEGERP #:G3402) (ODDP #:G3402)))
(build-call 'and '(integerp oddp)) ;; => (LAMBDA (#:G3405) (AND (INTEGERP #:G3405) (ODDP #:G3405)))
