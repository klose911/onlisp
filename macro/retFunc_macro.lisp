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

;;(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4)) ;; => 4

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

;; (mapcar (fn (and integerp oddp))
;; 	'(c 3 p 0)) ;; => (NIL T NIL NIL)
;; (mapcar (fn (or integerp symbolp))
;; 	'(c 3 p 0.2)) ;; => (T T T NIL)

;; (mapcar (fn (list 1- identity 1+)) 
;; 	'(1 2 3)) ;; ((0 1 2) (1 2 3) (2 3 4))
;; => #<FUNCTION :LAMBDA (#:G3499) (LIST (1- #:G3499) (IDENTITY #:G3499) (1+ #:G3499))>

;; (remove-if (fn (or (and integerp oddp)
;; 		   (and consp cdr)))
;; 	   '(1 (a b) c (d) 2 3.4 (e f g))) ;; => (C (D) 2 3.4)
;; => #<FUNCTION :LAMBDA (#:G3500)
;;  (OR ((LAMBDA (#:G3501) (AND (INTEGERP #:G3501) (ODDP #:G3501))) #:G3500)
;;   ((LAMBDA (#:G3502) (AND (CONSP #:G3502) (CDR #:G3502))) #:G3500))>

;; (1+ (find-if (fn oddp) '(2 3 4))) ;; 4

;; (fn (list (1+ truncate)))
;; #<FUNCTION :LAMBDA (#:G3488) (LIST ((LAMBDA (#:G3489) (1+ (TRUNCATE #:G3489))) #:G3488))>
;; (compose #'list #'1+ #'truncate) 

;; 15.2 cdr上做递归
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


;; alrec是lrec的一种包装
(defmacro alrec (rec &optional base)
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (labels ((rec () (funcall ,gfn)))
		 ,rec))
	   ,base)))
(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
	       (symbol-macrolet ((rec (funcall ,gfn)))
		 ,rec))
	   ,base)))
(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

(defun our-length(lst)
  (on-cdrs (1+ (rec)) 0 lst))

;; (our-length '(1 2 3)) ;; => 3 
;; (macroexpand-1 '(on-cdrs (1+ (rec)) 0 lst))
;; (FUNCALL (ALREC (1+ (REC)) #'(LAMBDA NIL 0)) LST)
;; (macroexpand-1 '(ALREC (1+ (REC)) #'(LAMBDA NIL 0)))
;; (LREC #'(LAMBDA (IT #:G3494)
;; 	  (LABELS ((REC nil
;; 		     (FUNCALL #:G3494)))
;; 	    (1+ (REC)))) ;; 这里的rec是个通过label的局部定义函数
;;       #'(LAMBDA NIL 0))
;; (funcall (lrec #'(lambda (x f)
;; 		   (1+ (funcall f))) 0) '(1 2 3 5))

(defun our-every (fn lst)
  (on-cdrs (and (funcall fn it) （rec)) t lst))

;; copy-list
(defun our-copy-list (lst)
  (on-cdrs (cons it (rec)) nil lst))

(defun our-remove-duplicates (lst)
  (on-cdrs (adjoin it (rec)) nil lst))

(defun our-find-if (fn lst)
  (on-cdrs (if (funcall fn it) it (rec)) nil lst))

(defun our-some (fn lst)
  (on-cdrs (or (funcall fn it) (rec)) nil lst))

;; 基于on-cdr的新工具
;; 并集 等价于 ((lambda (&rest args) (reduce #'union args)) ...)
(defun unions (&rest sets)
  (on-cdrs (union it (rec)) (car sets) (cdr sets)))
;;(union '(a b) (union '(b c) '(c d)))
;; (unions '(a b) '(b c) '(c d)) ;;  (C D A B)

;; 交集
(defun intersections (&rest sets)
  (unless (some #'null sets) ;; 在最开始的地方加入了对于宏参数的null测试；如果集合中存在空集，终止整个计算过程
    (on-cdrs (intersection it (rec)) (car sets) (cdr sets))))

;; 差集
(defun differences (set &rest outs)
  (on-cdrs (set-difference (rec) it) set outs))
;; (set-difference '(a b c d) '(a c)) ;; => (B D) 
;; (differences '(a b c d e) '(a f) '(d)) ;; => (B C E) 

;; 返回多值的函数
(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) (rec)
	       (values (max mx it) (min mn it)))
	     (values (car args) (car args))
	     (cdr args))))
;; (maxmin '(1 3 56 233 -1 -100)) ;; => 233, -100

;; 15.3 基于子树的递归

(defmacro atrec (rec &optional (base 'it))
  "cltl2 version"
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
	       (symbol-macrolet ((left (funcall ,lfn))
				 (right (funcall ,rfn)))
		 ,rec))
	   #'(lambda (it) ,base))))

(defmacro atrec (rec &optional (base 'it))
  "cltl1 version"
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
	       (labels ((left () (funcall ,lfn))
			(right () (funcall ,rfn)))
		 ,rec))
	   #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

(defun our-copy-tree (tree)
  (on-trees (cons left right) it tree))
;; (funcall (ttrav #'cons)
;; 	 '((a b) c)) ;; => ((A B) C)
;; (our-copy-tree '((a b) (e) f)) ;; => ((A B) (E) F)

(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))
;; (funcall (ttrav #'(lambda (left right)
;; 		    (+ left (or right 1)))
;; 		1)
;; 	 '((a b (c d)) (e) f)) ;; => 10
;; (count-leaves '((a b (c d)) (e) f)) ;; => 10

(defun mklist (obj)
  (if (listp obj) obj (list obj)))
(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))
;; (funcall (ttrav #'nconc #'mklist)
;; 	 '((a b (c d)) (e) f ())) ;; =>  (A B C D E F)
;; (flatten '((a b (c d)) (e) f ())) ;; =>  (A B C D E F)

(defun rfind-if (fn tree)
  (on-trees (or left right)
	    (and (funcall fn it) it)
	    tree))
;; (funcall (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
;; 	       #'(lambda (tree) (and (oddp tree) tree))) ;; base
;; 	 '(2 (3 4) 5)) ;; 3

;; (defun fint (fn &rest fns)
;;   (if (null fns)
;;       fn
;;       (let ((chain (apply #'fint fns)))
;;         #'(lambda (x) 
;;             (and (funcall fn x) (funcall chain x))))))

;; (rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5)) ;; 3 

;; 惰性求值 只有当你需要表达式值的时候，才去求值它 
;; 构造delay对象：代表着承诺，如果今后需要的话，就要给出表达式的值，同时这个承诺本身是个 Lisp 对象 

(defconstant unforced (gensym))  

;; delay结构体由两部分构成，第一个字段代表是否已经被求值了，如果是的话就被赋予这个值。第二个字段则是一个闭包，调用它就能得到该 delay 所代表的值
(defstruct delay forced closure) 

;; delay宏用来简化创建delpay结构体
(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
	     #'(lambda ()
		 (setf (delay-forced ,self) ,expr)))
       ,self)))

;; 函数 force 接受任意对象：对于普通对象它就是 identity 函数，但对于 delay，它是对 delay 所代表的值的请求。
(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
	  (funcall (delay-closure x))
	  (delay-forced x))
      x)) 

;; (force 'a)  ;; => A 

;; 无论何时，只要需要处理的对象有可能是 delay ，就应该用 force 对付它!!!! 
;;  (sort lst #'(lambda (x y) (> (force x) (force y)))) 
