;; 宏创建上下文(context)。任何令其参数在一个新的上下文环境里求值的操作符都必须被定义成宏
;; 一个操作符，如果其参数求值的次数少于一次或者多于一次，那么也同样必须被定义成宏。

;; 11.1 创建上下文　

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
			 (if (consp x) (car x) x))
		     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
		  (if (consp x) (cadr x) nil))
	      binds)))

(macroexpand-1 '(our-let ((x 1) (y 2))
		 (+ x y))) ;; ((lambda (x y) (+ x y)) 1 2)

(mapcar #'(lambda (x)
	    (if (consp x) (car x) x))
	'((x 1) (y 2)))  ;; -> (x y)

(mapcar #'(lambda (x)
	    (if (consp x) (cadr x) nil))
	'((x 1) (y 2)))  ;; -> (1 2)  

(our-let ((x 1)(y 2)) (+ x y)) ;; -> 3

;; when-bind 作为参数列表解构的示例 
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

;; when-bind* 接受一个由成对的 (symbol expression) form 所组成的列表, 就和 let 的第一个参数的形式相同。如果任何expression返回nil，那么整个when-bind*表达式就返回nil。同样，它的主体在每个符号像在let*` 里那样被绑定的情况下求值：
(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
	 (if ,(caar binds)
	     (when-bind* ,(cdr binds) ,@body)))))

(when-bind* ((x (find-if #'consp '(a (1 2) b)))
	     (y (find-if #'oddp x)))
	    (+ y 10)) ;; 11

(macroexpand-1 '(when-bind* ((x (find-if #'consp '(a (1 2) b)))
			     (y (find-if #'oddp x)))
		 (+ y 10)))

;; (LET ((X (FIND-IF #'CONSP '(A (1 2) B))))
;;   (IF X (WHEN-BIND* ((Y (FIND-IF #'ODDP X)))
;;	  (+ Y 10)))) 

(car '((x (find-if #'consp '(a (1 2) b)))
       (y (find-if #'oddp x)))) ;; (X (FIND-IF #'CONSP '(A (1 2) B)))

(FIND-IF #'CONSP '(A (1 2) B)) ;; (1 2)
(find-if #'oddp '(1 2)) ;; 1

;; 将整个变量列表绑定到 gensym 
(defmacro with-gensyms-1 (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))


(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
	(vars (mapcar #'(lambda (v) (cons v (gensym)))
		      (remove-duplicates
		       (mapcar #'car
			       (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
		,@body))
       (cond ,@(mapcar #'(lambda (cl)
			   (condlet-clause vars cl bodfn))
		       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
		(let ,(condlet-binds vars cl)
		  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
	      (if (consp bindform)
		  (cons (cdr (assoc (car bindform) vars))
			(cdr bindform))))
	  (cdr cl)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts))) 

;; (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
;;    ((= 1 1) (y (princ 'c)) (x (princ 'd)))
;;    (t (x (princ 'e)) (z (princ 'f))))
;;  (list x y z))  ;; -> (D C NIL) 

;; 11.2
;; 构造上下文的宏的名字经常以 with- 开始 
(with-open-file (s "dump" :direction :output)
  (princ 99 s)) ;; 它的主体和一个新打开的文件一起求值, 该表达式求值完毕以后，文件 "dump" 将自动关闭，它的内容将是两个字符 "99"。

;; 创建上下文的宏将被展开成一个代码块；附加的表达式可能被放在主体之前、之后，或者前后都有。如果是出现在主体之后，其目的可能是为了在结束时，让系统的状态保持一致 去做某些清理工作。

;; 将上下文创建的宏展开进一个 unwind-protect 里。unwind-protect 的目的是确保特定表达式被求值，甚至当执行被中断时。它接受一个或更多参数，这些参数按顺序执行。如果一切正常的话它将返回第一个参数的值，就像 prog1 。区别在于，即使当出现错误，或者抛出的异常中断了第一个参数的求值，其余的参数也一样会被求值。

(unwind-protect
     (progn (princ "What error?")
	    (error "This error."))
  (setq x 'b)) ;; (setq x 'b) 依旧会被求值　

;; with-open-file 展开成了一个 unwind-protect ，所以即使对 with-open-file 的 body 求值时发生了错误，它打开的文件还是会一如既往地被关闭。

;; with-db 也更安全，因为它会展开成 unwind-protect, 这样无论如何都会关闭数据库连接
(defmacro with-db-macro (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
	    (progn
	      (setq *db* ,db)
	      (lock *db*)
	      ,@body)
	 (progn
	   (release *db*)
	   (setq *db* ,temp))))))

;; 把函数和宏结合起来。当 with- 宏变得愈发复杂时，第二种方法更有实践意义。
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
    '(let ((,gbod #'(lambda () ,@body)))
      ;;dynamic-extent 声明使得在为含主体的闭包分配空间时，可以更高效一些( CLTL1 实现会忽略该声明)。我们只有在 with-db-fn 调用期间才需要这个闭包，该声明也正合乎这个要求，它允许编译器从栈上为其分配空间。这些空间将在let 表达式退出时自动回收，而不是之后由垃圾收集器回收。
      (declare (dynamic-extent ,gbod)) 
      (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
  (unwind-protect
       (progn
	 (setq *db* new-db)
	 (lock *db*)
	 (funcall body))
    (progn
      (release *db*)
      (setq *db* old-db))))

;; 11.3条件求值　

;; 定义一个三值逻辑的条件选择。这个宏不再将 nil 当成假，把除此之外的都作为真，而是考虑了三种真值类型：真，假，以及不确定，表示为 ?
(defmacro if3 (test t-case nil-case ?-case)
  `(case  ,test
     ((nil) ,nil-case)
     (?  ,?-case)
     (t  ,t-case)))　　

;; 接受数值表达式作为第一个参数，并根据这个表达式的符号来求值接下来三个参数中的一个 
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
	     ((zerop ,g) ,zero)
	     (t ,neg)))))

(mapcar #'(lambda (x)
	    (nif x 'p 'z 'n))
	'(0 1 -1)) ;; (Z P N)

;; in 把 member 的抽象与 or 的效率结合在了一起。
;; 简洁和高效两种习惯用法之间择一而从时，我们取中庸之道，方法是编写宏将前者变换成为后者。
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

;; inq 类似与　setq 和set 
(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
			  `',a)
		      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
			 `(funcall ,fnsym ,c))
		     choices)))))

;; 它会在比较之前先对每个子句里的键进行求值以外，其行为和 case 相同。
;; (名字中的 > 意指通常用来表示求值过程的那个箭头符号。) 因为 >case 使用了 in，只有它需要的那个键才会被求值。
(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
		       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
	  ((inq key t otherwise) `(t ,@rest))
	  (t (error "bad >case clause")))))

;; 11.4 迭代 

(defmacro while (test &body body)
  '(do ()
    ((not ,test))
    ,@body))

(defmacro till (test &body body)
  '(do ()
    (,test)
    ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    '(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
      ((> ,var ,gstop))
      ,@body)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

;; (map0-n #'1+ 5) ;; (1 2 3 4 5 6)  

(defun map1-n (fn n)
  (mapa-b fn 1 n)) 
;; (map1-n #'1+ 5) ;; (2 3 4 5 6) 

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
	`(prog ((,src ,source))
	    (mapc #'(lambda ,parms ,@body)
		  ,@(map0-n #'(lambda (n)
				`(nthcdr ,n ,src))
			    (1- (length parms) ))))))) 

(do-tuples/o (x) '(a b c) (princ x))

;; (macroexpand-1 ' (do-tuples/o (x y) (a b c d) (princ (list x y))) )  
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms-1 (src rest bodfn)
	(let ((len (length parms)))
	  `(let ((,src ,source))
	     (when (nthcdr ,(1- len) ,src)
	       (labels ((,bodfn ,parms ,@body))
		 (do ((,rest ,src (cdr ,rest)))
		     ((not (nthcdr ,(1- len) ,rest))
		      ,@(mapcar #'(lambda (args)
				    `(,bodfn ,@args))
				(dt-args len rest src))
		      nil)
		   (,bodfn ,@(map1-n #'(lambda (n)
					 `(nth ,(1- n)
					       ,rest))
				     len)))))))))) 
(do-tuples/c (x y) '(a b c d)  (princ (list x y)))  ;; (A B)(B C)(C D)(D A)  

;; (macroexpand-1 '(do-tuples/c (x y) '(a b c d) (princ (list x y)))) 

;; (LET ((#:G3838 '(A B C D)))
;;  (WHEN (NTHCDR 1 #:G3838)
;;  (LABELS ((#:G3840 (X Y) (PRINC (LIST X Y))))
;;   (DO ((#:G3839 #:G3838 (CDR #:G3839))) 
;;       ((NOT (NTHCDR 1 #:G3839)) (#:G3840 (NTH 0 #:G3839) (NTH 0 #:G3838)) NIL)
;;     (#:G3840 (NTH 0 #:G3839) (NTH 1 #:G3839)))))) 

;; #;G3838: source 
;; #;G3839: rest
;; #;G3840: bodyfn

(LET ((source '(A B C D)))
  (WHEN (NTHCDR 1 source)
    (LABELS ((bodyfn (X Y) (PRINC (LIST X Y))))
      (DO ((rest source (CDR rest))) 
	  ((NOT (NTHCDR 1 rest)) (bodyfn (NTH 0 rest) (NTH 0 source)) NIL)
	(bodyfn (NTH 0 rest) (NTH 1 rest)))))) 

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
	      (map1-n #'(lambda (n)
			  (let ((x (+ m n)))
			    (if (>= x len)
				`(nth ,(- x len) ,src)
				`(nth ,(1- x) ,rest))))
		      len))
	  (- len 2)))

;; 11.5 多值迭代 mydo* 
(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body)) 

(mvdo* ((x 1 (1+ x))
	((y z) (values 0 0) (values z x)))
       ((> x 5) (list x y z))
       (princ (list x y z)))  ;; (6 5 6) 

(macroexpand-1 
 '(mvdo* ((x 1 (1+ x))
	  ((y z) (values 0 0) (values z x)))
   ((> x 5) (list x y z))
   (princ (list x y z))) ) 

;; (LET ((X 1))
;; (MULTIPLE-VALUE-BIND (Y Z) (VALUES 0 0)
;;  (PROG NIL #:G3846 (IF (> X 5) 
;;         (RETURN (PROGN (LIST X Y Z)))) (PRINC (LIST X Y Z)) (SETQ X (1+ X))
;;   (MULTIPLE-VALUE-SETQ (Y Z) (VALUES Z X)) (GO #:G3846))))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
	`(prog nil
	    ,label
	    (if ,(car test)
		(return (progn ,@(cdr test))))
	    ,@body
	    ,@(mvdo-rebind-gen rebinds)
	    (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
	(let ((var/s (caar binds)) (expr (cadar binds)))
	  (if (atom var/s)
	      `(let ((,var/s ,expr)) ,rec)
	      `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
	((< (length (car rebinds)) 3)
	 (mvdo-rebind-gen (cdr rebinds)))
	(t
	 (cons (list 
		(if (atom (caar rebinds))
		    'setq
		    'multiple-value-setq)
		(caar rebinds)
		(third (car rebinds)))
	       (mvdo-rebind-gen (cdr rebinds)))))) 

;; (mvdo-rebind-gen '(((x y) a b))) ;; ((MULTIPLE-VALUE-SETQ (X Y) B)) 
;; (mvdo-rebind-gen '((x y a)))  ;; ((SETQ X A)) 


;; (mvdo* (((px py) (pos player) (move player mx my))
;; 	((x1 y1) (pos obj1) (move obj1 (- px x1)
;; 				  (- py y1)))
;; 	((x2 y2) (pos obj2) (move obj2 (- px x2)
;; 				  (- py y2)))
;; 	((mx my) (mouse-vector) (mouse-vector))
;; 	(win nil (touch obj1 obj2))
;; 	(lose nil (and (touch obj1 player)
;; 		       (touch obj2 player))))
;;        ((or win lose) (if win 'win 'lose))
;;        (clear)
;;        (draw obj1)
;;        (draw obj2)
;;        (draw player)) 

;; 11.6 需要宏的原因
;; 宏并不是保护参数免于求值的唯一方式。另一种方法是把它封装在闭包里。
;; 条件求值和重复求值的相似之处在于这两个问题在本质上都不需要宏。

(defun fnif (test then &optional else)
  (if test
      (funcall then)
      (if else
	  (funcall else))))

;; (fnif (rich)
;;   #'(lambda () (go-sailing))
;;   #'(lambda () (rob-bank))) ;; then和else参数表达成闭包

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
	 (syms (mapcar #'(lambda (p)
			   (mapcar #'(lambda (x) (gensym))
				   (mklist (car p))))
		       pairs)))
    (labels ((rec (ps ss)
	       (if (null ps)
		   `(setq
		     ,@(mapcan #'(lambda (p s)
				   (shuffle (mklist (car p))
					    s))
			       pairs syms))
		   (let ((body (rec (cdr ps) (cdr ss))))
		     (let ((var/s (caar ps))
			   (expr (cadar ps)))
		       (if (consp var/s)
			   `(multiple-value-bind ,(car ss)
				,expr
			      ,body)
			   `(let ((,@(car ss) ,expr))
			      ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
	((null y) x)
	(t (list* (car x) (car y)
		  (shuffle (cdr x) (cdr y))))))


(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
	(temps (mapcar #'(lambda (b)
			   (if (listp (car b))
			       (mapcar #'(lambda (x)
					   (gensym))
				       (car b))
			       (gensym)))
		       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
			      (list var (cadr b)))
			  binds
			  temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
		      (mappend #'mklist (mapcar #'car binds))
		      (mappend #'mklist temps))
	  ,label
	  (if ,test
	      (return (progn ,@result)))
	  ,@body
	  (mvpsetq ,@(mapcan #'(lambda (b)
				 (if (third b)
				     (list (car b)
					   (third b))))
			     binds))
	  (go ,label)))))
