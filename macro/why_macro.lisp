;;宏有两点是函数无法做到的：宏可以控制（或阻止） 对其参数的求值，并且它可以展开进入到主调方的上下文中。
;;宏能控制宏调用中参数的求值, 取决于参数在宏展开式中的位置，它们可以被求值一次，多次，或者根本不求值。
;; 1. 变换 setf宏，使用rplaca，还是rplacd，必须判断表达式是car还是cdr
;; 2. 绑定 setq宏，第一个参数不求值
;; 3. 条件求值 when宏
;; 4. 多重求值 do宏

;; 5. 利用调用方的环境
(defmacro foo (x)
  '(+ ,x y)) ;; y就是调用时候绑定的
;; 6. 创建新的环境 let宏

;; 7. 减少函数调用 使用inline函数代替 

;;8.2 宏VS函数

(defun avg-func (&rest args)
  (/ (apply #'+ args) (length args)))  

(defmacro avg-macro (&rest args)
  '(/ (+ ,@args) ,(length args))) 

;; 宏的优点 
;; 1. 编译器计算
;; 2. 和Lisp集成更好
;; 3. 免除函数调用，可以用inline代替 

;; 函数的优点
;; 1. 函数即数据, 函数可以当成参数传递（例如用 apply），被函数返回，或者保存在数据结构里 
;; 2. 代码清晰
;; 3. 运行清晰 
;; 4. 递归清晰 

;; 8.3 使用宏的场合
;; 句法转换, 减少代码量

;;使用函数实现的版本
(defun move-objs-func (objs dx dy)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (incf (obj-x o) dx)
      (incf (obj-y o) dy))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
	      (max x1 xb) (max y1 yb)))))

(defun scale-objs-func (objs factor)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (setf (obj-dx o) (* (obj-dx o) factor)
	    (obj-dy o) (* (obj-dy o) factor)))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
	      (max x1 xb) (max y1 yb))))) 

;;使用宏定义把具体操作无关的重绘细节抽象出来 
(defmacro with-redraw-macro ((var objs) &body body)
  (let ((gob (gensym))
	(x0 (gensym)) (y0 (gensym))
	(x1 (gensym)) (y1 (gensym)))
    '(let ((,gob ,objs))
      (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
        (dolist (,var ,gob) ,@body)
        (multiple-value-bind (xa ya xb yb) (bounds ,gob)
          (redraw (min ,x0 xa) (min ,y0 ya)
		  (max ,x1 xb) (max ,y1 yb)))))))

(defun move-objs (objs dx dy)
  (with-redraw-macro (o objs)
    (incf (obj-x o) dx)
    (incf (obj-y o) dy)))

(defun scale-objs (objs factor)
  (with-redraw-macro (o objs)
    (setf (obj-dx o) (* (obj-dx o) factor)
	  (obj-dy o) (* (obj-dy o) factor))))

;; 创建Lisp嵌入式语言 
;; 实现嵌入式语言的初级方式是用Lisp 给它写一个解释器。
;; 一个更好的方法是通过语法转换实现这种语言：将每个表达式转换成 Lisp 代码，然后让解释器可以通过求值的方式来运行它。这就是宏大展身手的时候了. 新语言里的表达式被转换成了 Lisp，那么 Lisp 编译器就会编译这些转换出来的代码。这样实现的语言不需要在运行期承受解释的开销。要是你还没有为你的语言编写一个真正编译器，宏会帮助你获得最优的性能。事实上，转换新语言的宏可以看作该语言的编译器 -- 只不过它的大部分工作是由已有的 Lisp 编译器完成的
