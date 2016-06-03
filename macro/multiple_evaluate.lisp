;;10.1 多重求值

(defmacro multi-evaluate-for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))　;; 作为stop传递的form 在每次迭代时都会被求值 。如果stop有副作用，就可能出错
     ,@body))

;;never stop 
;;(let ((x 2))
;;  (multi-evaluate-for (i 1 (incf x))
;;    (princ i)))

;; 在编写类似for的宏的时候，必须牢记：宏的参数是form，而非值。取决于它们出现在表达式中位置的不同，它们可能会被求值多次。在这种情况下，解决的办法是把变量绑定到stop form的返回值上，并在循环过程中引用这个变量。

;;10.2 求值顺序　
(defmacro wrong-evalue-order-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,gstop ,stop) ;;stop在var,start前面被求值，如果stop中的副作用修改了var,start绑定的变量则出错
	  (,var ,start (1+ ,var)))
	 ((> ,var ,gstop))
       ,@body)))

;; (let ((x 1))
;;  (wrong-evalue-order-for (i x (setq x 13))
;;			  (princ i))) ;; 13 -> NIL


;; 宏通常应该确保表达式求值的顺序和它们在宏调用中出现的顺序一致。

;;正确的版本　
(defmacro right-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
        (,gstop ,stop))
      ((> ,var ,gstop))
	,@body)))


(let ((x 1))
  (right-for (i x (setq x 13))
    (princ i))) ;; 12345678910111213 -> NIL 

;; 10.3 非函数式的展开器(宏带有副作用)
;; 展开器代码除其参数外不应依赖其他任何东西!!!

;;;;对 intern 的调用接受一个字符串，并返回对应的符号。如果我们省略了可选的包参数，它将在当前包里寻找符号
(defmacro string-call (opstring &rest args) 
  `(,(intern opstring) ,@args)) 
(defun our+ (x y) (+ x y))
(string-call "OUR+" 2 3)
;;　错误的原因在于：string-call将因此依赖于展开式生成时所在的包。如果our+在那个包里不可见，展开式将是一个对未知符号的调用

;; Common Lisp 并不保证绑定在 &rest 形参上的列表是新生成的。你不能破坏性地修改 &rest 形参
(defun et-al (&rest args)
  (nconc args (list 'et 'al)))

;; 不要试图通过破坏性修改宏参数中的列表数据， 宏展开器返回的表达式含有引用列表的话，就应该避免对它进行破坏性的操作！！
;; 10.4 递归宏　
(defun our-length (x)
  (if (null x)
      0
      (1+ (our-length (cdr x)))))　

(defun ntha (n lst)
  (if (= n 0)
    (car lst)
    (ntha (- n 1) (cdr lst))))

;; (defmacro nthb (n lst)
;;   `(if (= ,n 0)
;;     (car ,lst)
;;     (nthb (- ,n 1) (cdr ,lst)))) 

;; 函数版本ntha 之所以会终止因为它在 n 的值上递归，这个值在每次递归中减小。
;; 但是宏展开式只能访问到 form，而不是它们的值, nthb会被无限次展开！！！　
;; 更危险的是在解释器可以执行，但会造成编译的无限死循环！！！

;; 迭代版本的nth
(defmacro nthc (n lst)
  `(do ((n2 ,n (1- n2))
      (lst2 ,lst (cdr lst2)))
       ((= n2 0) (car lst2))))

;; 让宏展开成为一个对递归函数的调用。 
(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst)
  (if (= n 0)
    (car lst)
    (nth-fn (- n 1) (cdr lst))))

;; 展开式里的函数都用的是该展开式自己定制的版本。
(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst) ;; 内置的 labels special form会创建一个局部函数定义 
        (if (= n 0)
          (car lst)
          (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))

;; 可以写出一个宏，让它的展开式是递归生成的
(defmacro ora (&rest args)
  (or-expand args))

;; or-expand: 产生递归的展开式的函数
(defun or-expand (args)
  (if (null args)
    nil
    (let ((sym (gensym)))
      `(let ((,sym ,(car args)))
        (if ,sym
          ,sym
          ,(or-expand (cdr args))))))) 

;; 递归宏：这个与nthb不同，因为这是在参数的个数上递归，而不是直接用参数的值做递归
(defmacro orb (&rest args)
  (if (null args)
    nil
    (let ((sym (gensym)))
      `(let ((,sym ,(car args)))
        (if ,sym
          ,sym
          (orb ,@(cdr args)))))))

 
