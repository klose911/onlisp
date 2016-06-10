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

;; (compose #'list #'1+)
;; => #<FUNCTION :LAMBDA (&REST ARGS)
;;  (REDUCE #'FUNCALL FNS :FROM-END T :INITIAL-VALUE (APPLY FN1 ARGS))>
;; (car (last '(#'list #'1+)))  ;; => #'1+
;; (butlast '(#'list #'1+)) ;; (#'LIST)

;; #'(lambda (&rest args)
;;     (reduce #'funcall '(#'list #'1+) 
;; 	    :from-end t
;; 	    :initial-value (apply #'1+ args))) 
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

;; op函数名/宏，参数作用于所有的fns
(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
			  `(,(rbuild f) ,g))
		      fns)))))

;; compose用作操作符(operator)，我们就得到一个所有参数复合后得到的函数
(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		      (if fns
			  `(,(rbuild (car fns))
			     ,(rec (cdr fns)))
			  g)))
		(rec fns)))))

;; (fn '200) ;; => #<FUNCTION :LAMBDA (#:G3374) '(200 #:G3374)> 
;; (fn (lambda (x) (* x 2))) ;; => #<FUNCTION :LAMBDA (X) (* X 2)> 

;; (fn (and integerp oddp)) ;; => #<FUNCTION :LAMBDA (#:G3399) (AND (INTEGERP #:G3399) (ODDP #:G3399))> 
;; (rbuild '(and integerp oddp)) ;; => (LAMBDA (#:G3402) (AND (INTEGERP #:G3402) (ODDP #:G3402)))
;; (build-call 'and '(integerp oddp)) ;; => (LAMBDA (#:G3405) (AND (INTEGERP #:G3405) (ODDP #:G3405)))
;; (let ((g (gensym))) 
;;   `(lambda (,g)
;;      (AND ,@(mapcar #'(lambda (f)
;; 			`(,(rbuild f) ,g))
;; 		    '(integerp oddp))))) ;; => (LAMBDA (#:G3418) (AND (INTEGERP #:G3418) (ODDP #:G3418))) 

;; compose function 
;; (fn (compose list 1+ truncate)) ;; => #<FUNCTION :LAMBDA (#:G3368) (LIST (1+ (TRUNCATE #:G3368)))> 
;; (rbuild '(compose list 1+ truncate)) ;; => (LAMBDA (#:G3369) (LIST (1+ (TRUNCATE #:G3369))))
;; (cdr '(compose list 1+ truncate)) ;; => (LIST 1+ TRUNCATE)
;; (build-compose '(LIST 1+ TRUNCATE)) ;; => => (LAMBDA (#:G3370) (LIST (1+ (TRUNCATE #:G3370))))
;;  (let ((g (gensym))) 
;;     `(lambda (,g)
;;        ,(labels ((rec (fns)
;; 		      (if fns
;; 			  `(,(rbuild (car fns))
;; 			     ,(rec (cdr fns)))
;; 			  g)))
;; 		(rec '(LIST 1+ TRUNCATE))))) ;; => (LAMBDA (#:G3373) (LIST (1+ (TRUNCATE #:G3373))))

;; (fn (compose (lambda (x) (+ x 3)) truncate)) ;; => #<FUNCTION :LAMBDA (#:G3375) ((LAMBDA (X) (+ X 3)) (TRUNCATE #:G3375))>

(mapcar (fn (and integerp oddp))
	'(c 3 p 0)) ;; => (NIL T NIL NIL)
(mapcar (fn (or integerp symbolp))
	'(c 3 p 0.2)) ;; => (T T T NIL)

(mapcar (fn (list 1- identity 1+)) 
	'(1 2 3)) ;; ((0 1 2) (1 2 3) (2 3 4))
;; => #<FUNCTION :LAMBDA (#:G3499) (LIST (1- #:G3499) (IDENTITY #:G3499) (1+ #:G3499))>

(remove-if (fn (or (and integerp oddp)
		   (and consp cdr)))
	   '(1 (a b) c (d) 2 3.4 (e f g))) ;; => (C (D) 2 3.4)
;; => #<FUNCTION :LAMBDA (#:G3500)
;;  (OR ((LAMBDA (#:G3501) (AND (INTEGERP #:G3501) (ODDP #:G3501))) #:G3500)
;;   ((LAMBDA (#:G3502) (AND (CONSP #:G3502) (CDR #:G3502))) #:G3500))>

(1+ (find-if (fn oddp) '(2 3 4))) ;; 4

(fn (list (1+ truncate)))
;; #<FUNCTION :LAMBDA (#:G3488) (LIST ((LAMBDA (#:G3489) (1+ (TRUNCATE #:G3489))) #:G3488))>
(compose #'list #'1+ #'truncate) 

;; 15.2 crd上做递归
;; rec: rec必须是一个接受两个参数的函数，一个参数是列表的当前car，另一个参数是个函数，通过调用这个函数进行递归
(defun lrec (rec &optional base)
  (labels ((self (lst) 
	     (if (null lst)
		 (if (functionp base)
		     (funcall base) 
		     base)
		 (funcall rec (car lst) 
			  #'(lambda ()
			      (self (cdr lst)))))))
    #'self))  
;; (lrec #'(lambda (x f) (1+ (funcall f))) 0)

(defun our-length (lst)
  (on-cdrs (1+ rec) 0 lst))

(defun our-every (fn lst)
  (on-cdrs (and (funcall fn it) rec) t lst))

(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (symbol-macrolet ((rec (funcall ,gfn)))
		 ,rec))
	   ,base)))

;; (defmacro alrec (rec &optional base)
;;   "cltl1 version"
;;   (let ((gfn (gensym)))
;;     `(lrec #'(lambda (it ,gfn)
;;         (labels ((rec () (funcall ,gfn)))
;;           ,rec))
;;       ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

;; (macroexpand-1  '(on-cdrs (1+ rec) 0 lst)) 
