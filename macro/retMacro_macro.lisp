;; 16 定义宏的宏
;; 16.1 缩略语

;; 使用rest 和,@的惯用法，就已经能为任意一个函数、宏，或者special form定义其缩略语了
(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))
(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

;; 为了定义一个定义宏的宏，要用到嵌套的反引用

;; 把 multiple-value-bind从反引用里拉出来的话
;; (defmacro mvbind (&rest args)
;;   (let ((name 'multiple-value-bind))
;;     `(,name ,@args)))

;; 把可变的表达式替换成变量, mvbind->,short, multiple-value-bind -> ,long
;; `(defmacro ,short (&rest args)
;;   (let ((name ',long))
;;     `(,name ,@args)))

;; 把代表name的short，long从内层反引用中移到外层abrev的参数列表中，来简化表达式
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,`,long ,@args)))

;; (abbrev dbind destructuring-bind) 等价与用rest和,@来定义

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(abbrev ,@pair))
	       (group names 2))))

;; (abbrevs dbind destructuring-bind
;;   mvbind multiple-value-bind
;;   mvsetq multiple-value-setq) 

;; 16.2 属性 Lisp提供多种方式将属性和对象关联在一起
;; (setf (get o p) v)
;; (setf (get 'ball1 'color) 'red) 
(defmacro color (obj)
  `(get ,obj 'color))
;; (color 'ball1) => RED 
;; (setf (color 'ball1) 'green) => GREEN
;; 优势：能把程序表示对象颜色的方式隐藏起来。属性表的访问速度比较慢，可能会出于速度考虑，将颜色表示成结构体的一个字段，或者哈希表中的一个表项。
;; 如果通过类似color宏这样的外部接口访问数据，可以很轻易地对底层代码做翻天覆地的改动

(defmacro weight (obj)
  `(get ,obj 'weight))

;; (propmacro color) 被展开成上述的color宏
;; color宏等价于
;; (defmacro color (obj)
;;   (let ((p 'color)) 
;;     `(get ,obj ',p)))

;; 构造出的模板如下
;; `(defmacro ,propname (obj) 
;;    (let ((p ',propname)) 
;;     `(get ,obj ',p))) 

;; 把模板中的propname提取到propmacro的宏参数中，消除不必要的let赋值
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))
;; (propmacro color)
;; (macroexpand-1 '(propmacro color))
;;  (DEFMACRO COLOR (OBJ) (CONS 'GET (CONS OBJ '('COLOR)))) 
(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p)
			 props))))
;; 16.3 指代宏
;; (let ((res (complicated-query)))
;;   (if res
;;       (foo res)))
;; (aif (complicated-query)
;;   (foo it))

;; (let ((o (owner x)))
;;   (and o (let ((a (address o)))
;; 	   (and a (city a)))))
;; (aand (owner x) (address it) (city it))

;; a+expand 的一般策略是对宏调用中的参数列表不断地求 cdr，同时生成一系列嵌套的 let 表达式；每一个 let 都将 it 绑定到不同的参数上，但同时也把每个参数绑定到一个不同的生成符号上
;; 展开函数聚集出一个这些生成符号的列表，并且当到达参数列表的结尾时，它就返回一个以这些生成符号作为参数的+表达式
(defun a+expand (args syms)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(a+expand (cdr args)
		      (append syms (list sym)))))
      `(+ ,@syms)))
;; (a+expand '(a b c) nil )
;; (LET* ((#:G3492 A) (IT #:G3492))
;;   (LET* ((#:G3493 B) (IT #:G3493))
;;     (LET* ((#:G3494 C) (IT #:G3494))
;;       (+ #:G3492 #:G3493 #:G3494))))

(defmacro a+ (&rest args)
  (a+expand args nil))
;; (macroexpand-1 '(a+ 7.95 (* it .05) (* it 3))) 
;; (LET* ((#:G3495 7.95) (IT #:G3495))
;;   (LET* ((#:G3496 (* IT 0.05)) (IT #:G3496))
;;     (LET* ((#:G3497 (* IT 3)) (IT #:G3497))
;;       (+ #:G3495 #:G3496 #:G3497))))

;; Massachusetts 的餐饮税是 5%，而顾客经常按照这个税的三倍来计算小费
(defun mass-cost (menu-price)
  (a+ menu-price (* it .05) (* it 3))) 
;; (mass-cost 7.95) ;; => 9.54 套餐价格+税+小费，7.95 + 7.95 * 0.05 + 7.95 * 0.05 * 3  

(defmacro alist (&rest args)
  (alist-expand args nil))
(defun alist-expand (args syms)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(alist-expand (cdr args)
			  (append syms (list sym)))))
      `(list ,@syms)))
;;(alist 1 (+ 2 it) (+ 2 it)) => (1 3 5)


;; 从a+expand, alistexpand抽取出通用的函数 
(defun anaphex (args expr)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(anaphex (cdr args)
		     (append expr (list sym)))))
      expr))
;; 由anaphex函数定义的a+宏
;; (defmacro a+ (&rest args)
;;   (anaphex args '(+)))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))
;; (pop-symbol 'a+) ;; => +, :INHERITED

;; 将其参数前面的第一个字母(假设是一个a)拉出来以决定在最后的展开式里调用什么函数
(defmacro defanaph (name &optional calls)
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))
;; (defanaph a+) ;; => a+ 调用+函数
;; (mass-cost 7.95) ;; => 9.54
;; (defanaph alist) => alist 调用list函数

;; defanaph的局限
;; 1 只能工作在其参数全部求值的操作符
;; 2 在宏展开中，it总被绑定在前一个参数上。在某些场合, 例如awhen想要it始终绑在第一个参数的值上

;; 非递归版本 不会全部求值，只绑定在第一个参数的值
;; (defun anaphex2 (op args)
;;   `(let ((it ,(car args)))
;;      (,op it ,@(cdr args))))
;; (defmacro aif (&rest args)
;;  (anaphex2 'if args))

;; 3 无法工作在像setf这种期望其第一个参数是广义变量的宏上

;; (defun anaphex3 (op args)
;;   `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))
;; (defmacro asetf (&rest args)
;;   (anaphex3 '(lambda (x y) (declare (ignore x)) y) args))

(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	  ,(anaphex1 (cdr args)
		     (append call (list sym)))))
      call))

(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defmacro _f (op place &rest args)
  (let ((g (gensym)))
    (multiple-value-bind (vars forms var set access)
	(get-setf-expansion place)
      `(let* ((,g ,op)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (funcall ,g ,access ,@args)))
	 ,set))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defmacro defanaph-ext (name &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
	 (body (case rule
		 ;; :all 宏展开将采用alist 模型。宏调用中所有参数都将被求值，同时it总是被绑定在前一个参数的值上
		 (:all `(anaphex1 args '(,opname)))
		 ;; :first 宏展开将采用aif 模型。只有第一个参数是必须求值的，并且it将被绑定在这个值上。
		 (:first `(anaphex2 ',opname args))
		 ;; :place 第一个参数被按照广义变量来对待，而it将被绑定在它的初始值上
		 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
      ,body))) 

;; (defanaph-ext alist)

;; (defanaph-ext aif :rule first)

;; (defanaph-ext asetf :rule :place)
;; (defmacro incf-1 (place &optional (val 1))
;;   `(asetf ,place (+ it ,val)))
;; (defmacro pull (obj place &rest args)
;;   `(asetf ,place (delete ,obj it ,@args)))
