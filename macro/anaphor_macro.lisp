;; 14 指代宏  希望检查一个表达式的返回值是否为非空，如果是的话，使用这个值做某些事
;; 14.1 指代(anaphor) 是一种引用对话中曾提及事物的表达方式  
;; 代词，实际上是一种可捕捉的符号。我们可以通过指定某些符号，让它们充当代词，然后再编写宏有意地捕捉这些符号，用这种方式来使用代词

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))
(macroexpand-1 '(aif (big-long-calculation)
		 (foo it))) ;; (aif (big-long-calculation) (foo it))  在宏调用中，it 看起来是自由的，
;; (LET ((IT (BIG-LONG-CALCULATION))) 但事实上在 aif 展开时，表达式 (foo it) 会被插入到一个上下文中，而 it 的绑定就位于该上下文
;;   (IF IT (FOO IT) NIL))

;; when的指代版本 
(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))
(macroexpand-1 '(awhen (big-long-calculation)
		 (foo it)
		 (bar it)))
;; (AIF (BIG-LONG-CALCULATION)
;;      (PROGN (FOO IT)
;; 	    (BAR IT)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

;; 如果一个程序需要等待(poll) 某个外部数据源的话，类似 while 和 awhile 这样的宏就可以派上用场了
;; 而且，如果你在等待一个数据源，除非你想做的仅是静待它改变状态，否则你肯定会想用从数据源那里获得的数据做些什么
(macroexpand-1 '(awhile (poll *fridge*)
		 (eat it)))
;; (DO ((IT (POLL *FRIDGE*) (POLL *FRIDGE*)))
;;     ((NOT IT)) (EAT IT))


;; and 的指代版本；每次求值它的实参，it 都将被绑定到前一个参数返回的值上
;; 在实践中，aand 倾向于在那些做条件查询的程序
(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))
;; 如果一个宏总是产生包含对其自身调用的展开式，那么展开过程将永不终止。虽然 aand 是递归的，但是它却没有这个问题，因为在基本情形里它的展开式没有引用 aand !!!!

(macroexpand-1 '(aand (owner x)
		 (address it)
		 (town it)))
;; (AIF (OWNER X)
;;      (AAND (ADDRESS IT) (TOWN IT)))
;; (let ((own (owner x)))
;;   (if own
;;     (let ((adr (address own)))
;;       (if adr (town adr)))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))  ;; 测试结果一开始时将被保存在一个由 gensym 生成的变量里
	    (sym (gensym)))      ;; 目的是为了让符号 it 的绑定只在子句的其余部分有效
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym)) ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

(macroexpand-1 '(acond ((owner x) (keep it))
		 ((lover x) (marry it))
		 (t (nop))))
;; (LET ((#:G3455 (OWNER X)))
;;   (IF #:G3455 (LET ((IT #:G3455)) (KEEP IT))
;;       (ACOND ((LOVER X) (MARRY IT)) (T (NOP)))))

(defmacro acond-wrong (&rest clauses) 
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)))
	`(let ((it ,(car cl1))) ;; 只在第一次做测试的时候保存it 
	   (if it
	       (progn ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

(macroexpand-1 '(acond-wrong ((owner x) (keep it))
		 ((lover x) (marry it))
		 (t (nop))))
;; (LET ((IT (OWNER X)))
;;   (IF IT (PROGN (KEEP IT))
;;       (ACOND ((LOVER X) (MARRY IT)) (T (NOP)))))

;; 不能直接用lambda–表达式来表达递归函数
#'(lambda (x) (* x 2))
(apply #'(lambda (x) (* x 2)) '(100)) ;; => 200
(funcall #'(lambda (x) (* x 2)) 100) ;; => 200

;; 代替的方法是，你必须借助 labels 定义一个局部函数
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
	     (if (consp lst)
		 (+ (if (eq (car lst) obj) 1 0)
		    (instances-in (cdr lst)))
		 0)))
    (mapcar #'instances-in lsts)))
(count-instances 'a
		 '((a b c) (d a r p a) (d a r) (a a))) ;; => (1 2 1 2)

;; 字面引用递归函数
;; alambda 实例会展开进一个 labels 表达式，在这个表达式中，self 被绑定到正在定义的函数上
;; alambda 表达式不但更短小，而且看起来很像我们熟悉的 lambda 表达式
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))
(macroexpand-1 '(alambda (x)
		 (if (= x 0) 1 (* x (self (1- x)))))) 
;; (LABELS ((SELF (X)
;; 	   (IF (= X 0) 1
;; 	       (* X (SELF (1- X)))))) #'SELF)
(funcall (alambda (x)
	   (if (= x 0) 1 (* x (self (1- x))))) 10) ;; =>3628800
(apply (alambda (x)
	 (if (= x 0) 1 (* x (self (1- x))))) '(10)) ;; => 3628800


(defun count-instances-self (obj lists)
  (mapcar (alambda (lst) 
	    (if (consp lst)
		(+ (if (eq (car lst) obj)  1 0)
		   (self (cdr lst)))
		0))
	  lists))
(count-instances-self 'a
		      '((a b c) (d a r p a) (d a r) (a a))) ;; => (1 2 1 2)

;; 参数从左到右求值, 每次求值时变量it都会被绑定到前一个表达式的值上。
(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
		 (case (length args)
		   (0 nil)
		   (1 (car args))
		   (t `(let ((it ,(car args)))
			 ,(self (cdr args))))))
	       args)))

;; 把本可以被写得优雅漂亮的函数式程序弄成命令式程序的样子 
(macroexpand-1 '(ablock north-pole
		 (princ "ho ")
		 (princ it)
		 (princ it)
		 (return-from north-pole)))
;; (BLOCK NORTH-POLE
;;   (LET ((IT (PRINC "ho ")))
;;     (LET ((IT (PRINC IT)))
;;       (LET ((IT (PRINC IT)))
;; 	(RETURN-FROM NORTH-POLE)))))

;; 如果一个宏，它有意地使用了变量捕捉，那么无论何时这个宏被导出到另一个包的时候，都必须同时导出那些被捕捉了的符号。例如，无论 aif 被导出到哪里，it 也应该同样被导出到同样的地方。否则出现在宏定义里的it 和宏调用里使用的 it 将会是不同的符号!!!!!!!!!!!!!!

;; 14.2 失败
;; NIL 首先是一个空列表, 其次是逻辑假，最后函数返回 nil 以示失败
(find-if #'oddp '(2 4 6)) ;; => NIL 
(find-if #'null '(2 nil 6)) ;; => NIL  find-if 成功返回，而成功的原因是它发现了 nil

;; i 在多重返回值出现之前，最常用的方法是专门返回一个列表结构
(setq synonyms '((yes . t) (no . nil))) ;;=> ((YES . T) (NO))
(assoc 'no synonyms) ;; => (NO)
;; 返回以该元素开始的整个 cdr 
(member-if #'null '(2 nil 6)) ;; => (NIL 6)

;; ii 让访问函数接受一个特殊对象作为参数，一般是用个 gensym，然后在失败的时候返回这个对象
;; 这种方法被用于 get ，它接受一个可选参数来表示当特定属性没有找到时返回的东西
(get 'life 'meaning (gensym)) ;; => #:G3493

;; iii 多重返回值， 第一个返回值是数据，第二个区别成功或者失败
(setf edible (make-hash-table)
      (gethash 'olive-oil edible) t
      (gethash 'motor-oil edible) nil) ;; => NIL

(gethash 'motor-oil edible) ;; => NIL, T  第二个返回值表示找到

(defun edible? (x)
  (multiple-value-bind (val found?) (gethash x edible)
    (if found?
	(if val 'yes 'no)
	'maybe)))
(mapcar #'edible? '(motor-oil olive-oil iguana)) ;; => (NO YES MAYBE)

;; 通常采用gethash 的方式会更好一些！！！

;; 多重求值版本指代宏
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test ;; 绑定和测试的对象不再是同一个值，而是绑定第一个值，并测试第二个值 
       (if (or it ,win) ,then ,else))))
(macroexpand-1 '(aif2 (gethash x edible)
		 (if it 'yes 'no)
		 'maybe))
;; (MULTIPLE-VALUE-BIND (IT #:G3496)
;;     (GETHASH X EDIBLE)
;;   (IF (OR IT #:G3496)
;;       (IF IT 'YES 'NO) 'MAYBE)) 
(defun edible-aif2？ (x)
  (aif2 (gethash x edible)
	(if it 'yes 'no)
	'maybe)) 
(mapcar #'edible-aif2？ '(motor-oil olive-oil iguana))  ;; => (NO YES MAYBE)

(defmacro awhen2 (test &body body)
  `(aif2 ,test
	 (progn ,@body)))


(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
	 (aif2 ,test
	       (progn ,@body)
	       (setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (val (gensym))
	    (win (gensym)))
	`(multiple-value-bind (,val ,win) ,(car cl1)
	   (if (or ,val ,win)
	       (let ((it ,val)) ,@(cdr cl1))
	       (acond2 ,@(cdr clauses))))))) 


(let ((g (gensym))) ;; 把一个gensym传给read，万一遇到eof就返回它，免去了每次调用read2时构造gensym的麻烦
  (defun read2 (&optional (str *standard-input*)) 
    (let ((val (read str nil g))) ;; 内置的read指示错误的方式和get一样，接受一个可选参数来说明在遇到eof时是否报错，如果不报错的话，将返回读取到的值 
      (unless (equal val g) ;; 读取报错，g和nil 
	(values val t)))))  ;; 读取成功，read2返回两个值，读取的value和标志位t 

;; do-file宏方便地遍历一个文件里的所有表达式
(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
      (awhile2 (read2 ,str)
	,@body))))

(defun our-load (filename)
  (do-file filename (eval it)))

;; 14.3 引用透明 这个标准针对的是语言，而不是程序
;; a 任意一个子表达式都可以替换成另一个子表达式，只要后者和前者的值相等
;; b 在给定的上下文中，出现不同地方的同一表达式其取值都相同。

;; 第一个和最后一个x带有不同的值，因为被一个setq干预了, 所以lisp是引用不透明的
(list x
  (setq x (not x))
  x) 

;; 它重定义了if，而if的本意并非是被用来创建新的上下文的。我们可以给指代宏取个自己的名字
;; aif确实违背了另一个原则，它和引用透明无关：即，不管用什么办法，新建立的变量都应该在源代码里能很容易地分辨出来 
(defmacro if-convinient (test then &optional else)
  `(let ((that ,test))
     (if that ,then ,else)))

