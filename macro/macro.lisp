;;函数只产生结果，而宏却产生表达式。当它被求值时，才会产生结果。 

(defmacro nil! (var)
  (list 'setq var nil)) 

;; 宏产生的表达式将在调用宏的位置求值。宏调用是一个列表，列表的第一个元素是宏的名称。
;; 当我们把宏调用 (nil! x) 输入到 toplevel 的时候发生了什么? 
;;Lisp 首先会发觉 nil! 是个宏的名字，然后:
;;按照上述定义宏展开，宏展开式是 (setq x nil)。
;;在调用宏的地方求值该表达式。
;; 求值并不总是立即发生在展开之后。一个发生在函数定义里的宏调用将在函数编译时展开，但展开式要等到函数被调用时才会求值。

;; 只有在反引用和逗号 , 以及 comma-at ,@ 一同出现时才变得有用。如果说反引用创建了一个模板，那么逗号就在反引用中创建了一个槽(slot) 。一个反引用列表等价于将其元素引用起来  


(setq a 1 b 2 c 3)
`(a (,b c)) ;; (A (2 C)) 
`(a b ,c (',(+ a b c)) (+ a b) 'c '((,a 'b))) ;; (A B 3 ('6) (+ A B) 'C '((1 'B))) 

;;7.2 nil! 使用反引号版本
(defmacro nil! (var)
  `(setq ,var nil))  

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg))) 

;; combine ` with @
;; 带&的参数是作为列表传入函数的，@符号可以脱掉这个列表的括号，变成原来的样子 

(setq b '(1 2 3)) 
`(a ,b c) ;; (A (1 2 3) C) 
`(a ,@b c) ;;( A 1 2 3 C)

(defmacro our-when (test &body body)
  `(if ,test
       (progn
	 ,@body))) 

;; 7.3定义简单宏

;; 1. 从你想要定义的这个宏的一次典型调用开始。先把它写在纸上，然后下面写上它应该展开成的表达式。
;; 2. 通过宏调用，构造出你这个宏的参数列表，同时给每个参数命名。

;; 3. 现在回到之前写下的两个表达式。对于宏调用中的每个参数，画一条线把它和它在展开式里出现的位置连起来。
;; 4. 为了写出宏的实体，把你的注意力转移到展开式。让主体以反引用开头。
;; 5. 现在，开始逐个表达式地阅读展开式。每当发现一个括号，如果它不是宏调用中实参的一部分，就把它放在宏定义里。
;; 6. 所以紧接着反引用会有一个左括号。对于展开式里的每个表达式
;; 7. 如果没有线将它和宏调用相连，那么就把表达式本身写下来。
;; 8. 如果存在一条跟宏调用中某个参数的连接，就把出现在宏参数列表的对应位置的那个符号写下来，前置一个逗号。
;;如果在一系列展开式中的表达式和宏调用里的一系列形参之间存在联系，那么就把对应的 &rest 或 &body 实参记下来，在前面加上 comma-at。

;; (defmacro memq (obj lst)) 

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq)) 

(defmacro our-while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;;(pprint (macroexpand-1 '(while (able) (laugh)))) 一级展开宏 

;;7.5 参数列表的解构
(destructuring-bind (x (y) . z) '(a (b) c d)
  (list x y z)) 
;; 匹配模式  (x (y) . z) 
;; 一个求值到列表的实参 '(a (b) c d) 
;;求值表达式体  (list x y z)
;;在求值表达式时将模式中的参数绑定到列表的对应元素上： x->a, y->b, z->(c d) 
;; (list 'a 'b '(c d)) -> ( A B (C D)) 

;; 宏通常把参数包在一个列表里面，而后者不属于宏体
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
	   ,list)
     (let ((,var nil))
       ,result)))

;;(our-dolist (x '(a b c))
;;  (print x))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body))) 

;;(when-bind (input (get-user-input))
;;  (process input))

;; 7.6 

(defmacro our-expander (name) `(get ,name 'expander))

(our-expander 'hello) 

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
	     #'(lambda (,g)
		 (block ,name
		   (destructuring-bind ,parms (cdr ,g)
		     ,@body))))
       ',name)))

(let ((op 'setq))
  (our-defmacro  our-setq (var val)
		 (list op var val)))

(our-setq a 100) 


(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))

;; 7.7 our do macro 
(defun make-initforms (bindforms) 
  (mapcar #'(lambda (b) 
	      (if (consp b) 
		  (list (car b) (cadr b)) 
		  (list b nil))) 
	  bindforms))

(make-initforms '((w 3) (x 1 (1+ x)) (y 2 (1+ y)) (z))) ;; ((W 3) (X 1) (Y 2) (Z NIL))

(defun make-stepforms (bindforms) 
  (mapcan #'(lambda (b) 
	      (if (and (consp b) (third b))  
		  (list (car b) (third b)) 
		  nil)) 
	  bindforms))  

(make-stepforms '((w 3) (x 1 (1+ x)) (y 2 (1+ y)) (z))) ;; (X (1+ X) Y (1+ Y)) 

(defmacro our-do (bindforms (test &rest result) &body body)  
  (let ((label (gensym))) ;;符号重名
    `(prog ,(make-initforms bindforms)  
	,label 
	(if ,test 
	    (return (progn ,@result))) 
	,@body
	(psetq ,@(make-stepforms bindforms)) ;;同时求值 
	(go ,label))))
   
#|(our-do ((w 3)
    (x 1 (1+ x))
    (y 2 (1+ y))
    (z))
  ((> x 10) (princ z) y)
  (princ x)
  (princ y))|#

;; 7.8 宏风格
;; 展开器风格: 宏用它来生成其展开式,  重视代码的结构清晰可读 
;; 展开式代码: 现在展开式本身的代码中, 对效率的要求高一些 

;; 展开器代码是递归的 
;;为一个宏定义来说，our-and 即使算不上好，至少还过得去。尽管每次递归都调用 length ，这样可能会比较没效率，但是其代码的组织方式更加清晰地说明了其展开式跟 and 的连接词数量之间的依赖关系。
(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t '(if ,(car args)
        (our-and ,@(cdr args)))))) 

(defmacro our-andb (&rest args)
  (if (null args)
    t
    (labels ((expander (rest)
          (if (cdr rest)
            '(if ,(car rest)
              ,(expander (cdr rest)))
            (car rest))))
      (expander args))))

;; 展开式代码来说，正好相反。对宏展开式来说，代码可读与否不太重要，因为很少有人会去读它，而别人读这种代码的可能性更是微乎其微。平时严禁使用的 goto 在展开式里可以网开一面，备受冷眼的 setq 也可以稍微抬起头来。
;;宏通常用于实现通用的实用工具，这些工具会出现在程序的每个角落。如此频繁使用的代码是无法忍受低效的。一个宏，虽然看上去小小的，安全无害，但是在所有对它的调用都展开之后，可能会占据你程序的相当篇幅。
;;展开式代码避免使用cons 

;; 7.9 
;; 类似 while 的通用宏放在单独的文件里, 以便保证宏定义被首先编译
;; 某些宏只是为了用在程序的某个特定部分而写的, 这种宏应该跟使用它们的代码放在一起 

;; 7.10 
;; 函数转化为宏的难度取决于该函数的一些特性。最容易转化的一类函数有下面几个特点：
;; 1.  函数体只有一个表达式。
;; 2.  参数列表只由参数名组成。
;; 3.  不创建任何新变量（参数除外）。
;; 4.  不是递归的（也不属于任何相互递归的函数组）。
;; 5.  每个参数在函数体里只出现一次。
;; 6.  没有一个参数，它的值会在其参数列表之前的另一个参数出现之前被用到。
;; 7.  无自由变量 

(defun second-macor (x) 
  `(cadr ,x)) ;; 它不能作为 apply 或者 funcall 的第一个参数，而且被它调用的函数不能拥有局部绑定。

(defmacro noisy-second-macro (x)
  '(progn
    (princ "Someone is taking a cadr!") ;;多余一个表达式，你必须加上一个 progn
    (cadr ,x))) 

(defmacro sum-macro (&rest args) 
  `(+ ,@args)) ;;有 &rest 或者 &body 参数, 使用comma-@ 

(defmacro foo-macro (x y z)
  '(list ,x (let ((x ,y))
      (list x ,z)))) ;; 不能在参数列表里的所有符号前面放逗号了，取而代之，我们只把逗号加在那些引用了参数的符号前面 

;; 
(symbol-macrolet ((hi (progn (print "Howdy")
        1)))
  (+ hi 2))


