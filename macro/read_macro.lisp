;; 17 读取宏
;; 在Lisp表达式的一生中，有三个最重要的时刻，分别是读取期，编译期和运行期
;; 运行期: 函数左右
;; 编译期: 宏转换代码
;; 读取期: 读取宏起作用 

;; 17.1 宏字符
;; reader的行为是由那些可随时改变的属性和变量控制的, 若要改变其行为，最简单的方式就是定义新的宏字符
;; 宏字符是一种被 Lisp reader特殊对待的字符
;; 举个例子，小写字母 a 的处理方式和小写字母 b 是一样的，它们都由常规的处理方式处理
;; 但左括号就有些不同：它告诉Lisp开始读取一个列表!!!!!! 
;; 每个这样的字符都有一个与之关联的函数，告诉Lisp reader当遇到该字符的时候做什么
;; 可以改变一个已有的宏字符的关联函数，或者定义你自己的新的宏字符
;; 内置函数set-macro-character提供了一种定义读取宏的方式。它接受一个字符和一个函数，以后当read遇到这个字符时，它就返回调用该函数的结果

;; 单引号的读取宏 
(set-macro-character #\'
		     #'(lambda (stream char)
			  ;; 该函数忽略了它的第二个形参，因为它总是那个引用字符'!!!!
			 (declare (ignore char))
			 ;; read的最后三个参数：是否在碰到end-of-file时报错
			 ;;                  如果不报错的话返回什么值，
			 ;;                  read调用是否是发生在递归read调用中
			 (list 'quote (read stream t nil t))))
;; (defmacro q (s)
;;   (list 'quote s))
;; (q a) 
;; (defmacro q-1 (s)
;;   `(quote ,s))
;; (q-1 a)

;; 读取宏和常规宏一样，其实质都是函数
;; 和生成宏展开的函数一样，和宏字符相关的函数，除了作用于它读取的流以外，不应该再有其他副作用!!!!

;; 宏和读取宏在不同的阶段分析和观察你的程序
;; 宏在程序中发生作用时，它已经被reader解析成了 Lisp 对象
;; 而读取宏在程序还是文本的阶段时，就对它施加影响了!!!!!!

;; 读取宏至少在两方面比常规宏更为强大。读取宏可以影响Lisp读取的每一样东西，而宏只是在代码里被展开
;; 读取宏通常递归地调用read
;; ''a 会被读取宏变成 (quote (quote a))
;; (q (q a)) 则会变成 (Q A) !!!!! 解决这个问题的正确方法是定义一个编译器宏

;; 17.2 dispatching宏字符
;; #'和其他#开头的读取宏一样，是一种称为dispatching读取宏的实例。这些读取宏以两个字符出现，其中第一个字符称为dispatch 字符
;; 这类宏的目的，简单说就是尽可能地充分利用acii字符集；如果只有单字符读取宏的话，那么读取宏的数量就会受限于字符集的大小

;; #?宏用于定义常数函数的读取宏 
(set-dispatch-macro-character #\# #\?
  #'(lambda (stream char1 char2)
    (declare (ignore char1 char2))
    `#'(lambda (&rest ,(gensym))
	 ,(read stream t nil t))))

;; #?2 被定义成返回常数2的函数
;; (mapcar #?2 '(a b c)) => (2 2 2)
;; 早#？宏字符的定义中使用宏字符是完全没有问题的，表达式被读取后这些宏字符就消失了
;; (eq (funcall #?'a) 'a) => T 
;; (eq (funcall #?#'oddp) (symbol-function 'oddp)) => T

;; 17.3 定界符
;; 定义形如#[x y]的表达式，使得这样的表达式被读取为在x到y的闭区间上所有整数的列表
(set-macro-character #\] (get-macro-character #\)))
  (set-dispatch-macro-character #\# #\[
   #'(lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((accum nil)
	   ;; read-delimited-list函数的第一个参数是那个被当作列表结尾的字符]
        (pair (read-delimited-list #\] stream t)))
      (do ((i (ceiling (car pair)) (1+ i)))
        ((> i (floor (cadr pair)))
           (list 'quote (nreverse accum)))
	(push i accum)))))
;; #[2 7] => (2 3 4 5 6 7)

;; 宏 defdelim 接受两个字符，一个参数列表，以及一个代码主体
;; 参数列表和代码主体隐式地定义了一个函数
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
    ;; 读取到第二个字符为止
    (set-macro-character right rpar)
    ;; 首个字符定义为dispatching读取宏
    (set-dispatch-macro-character #\# left
				  #'(lambda (stream char1 char2)
				      (declare (ignore char1 char2))
				      ;; 将函数fn应用到它读到内容
				      (apply fn
					     (read-delimited-list right stream t))))))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

;; 这个定义和上面定义边界符[]的宏等价
(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))
;; #[1 5] => (1 2 3 4 5)

;; 复合像 list 和 1+ 这样的内置函数时，没有理由等到运行期才去对compose的调用求值
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns 
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

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

(defmacro fn (expr)
  `#',(rbuild expr))

;; 编译器生成复合函数！！！！ 
(defdelim  #\{ #\}  (&rest args)
  `(fn  (compose  ,@args)))
;; #{list 1+} ;;  #<FUNCTION :LAMBDA (#:G3497) (LIST (1+ #:G3497))>
;; (funcall #{list 1+} 7) ;; => (8)

;; 17.4 这些发生于何时
;; 读取宏是在常规宏之前作用的话，那么宏是怎样展开成含有读取宏的表达式的呢?
(defmacro quotable ()
  `(list 'able))
;; 真相是：这个宏定义中的两个引用在这个defmacro表达式被读取时，就都被展开了，展开结果如下
(defmacro quotable ()
  (quote (list (quote able))))
;; 在宏展开式里包含读取宏是没有什么问题的。因为一个读取宏的定义在读取期和编译期之间将不会（或者说不应该）发生变化!!!
