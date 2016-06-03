;; 变量捕捉发生在宏展开导致名字冲突的时候. 名字冲突指：某些符号结果出乎意料地引用了来自另一个上下文中的变量.

;; 9.1 宏参数捕捉
;; 宏参数捕捉，就是在宏调用中作为参数传递的符号无意地引用到了宏展开式本身建立的变量
(defmacro wrong-for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

(wrong-for (x 1 5)
  (princ x))

;;(for (limit 1 5)
;;  (princ limit))

(macroexpand-1 '(wrong-for (limit 1 5)
		 (princ limit)))
;; (DO ((LIMIT 1 (1+ LIMIT)) (LIMIT 5)) ((> LIMIT LIMIT)) (PRINC LIMIT))

;;在宏展开式本身的符号和作为参数传递给宏的符号之间出现了名字冲突。宏展开捕捉了 limit


;; (let ((limit 5))
;;  (for (i 1 10)
;;    (when (> i limit)
;;      (princ i))))

;; 9.2 自由符号捕捉
;; 宏定义本身有这么一些符号，它们在宏展开时无意中却引用到了其所在环境中的绑定

(defvar w nil)

(defmacro gripe (warning) 
  `(progn (setq w (nconc w (list ,warning)))
	  nil))

(defun sample-ratio (v w)
  (let ((vn (length v)) (wn (length w)))
    (if (or (< vn 2) (< wn 2))
	;; 使用 gripe 时的上下文含有 w 自己的局部绑定。所以，产生的警告没能保存到全局的警告列表里，而是被 nconc 连接到了 sample-ratio 的一个参数的结尾。不但警告丢失了，而且这个局部绑定的ｗ 也加上了一个多余的字符串
	(gripe "sample < 2") 
	(/ vn wn))))

(let ((lst '(b)))
  (sample-ratio nil lst)
  lst) ;;  (B "sample < 2")

;; 
(let ((x y) (z 10))
  (list w x z)) ;;ｗ，ｙ是自由的　

(let ((x x))
  x)　;; 第二个ｘ是自由的

(defmacro foo (x y)
  `(/ (+ ,x 1) ,y))

(macroexpand-1 '(foo (- 5 2) 6)) ;;  (/ (+ (- 5 2) 1) 6)
;;展开式的框架 (/ (+ 1) ) 我们需关心那些框架变量的绑定

;; 可捕捉(capturable)：如果一个符号满足下面条件之一，那就可以认为它在某些宏展开里是可捕捉的

;; 1.  它作为自由符号出现在宏展开式的框架里
;; (defmacro cap1 ()
;;   `(+ x 1))

;; 2. 它被绑定到框架的一部分，而该框架中含有传递给宏的参数，这些参数被绑定或被求值。
;; (defmacro cap2 (var)
;;  '(let ((x ...)
;;      (,var ...))
;;    ...))

;; 9.4 取更好的名字避免捕捉, 全局变量 *var_name*

;; 9.5 通过预先求值避免捕捉
;;只避免了seq的一次被求值
(defmacro wrong-before (x y seq)
  `(let ((seq ,seq))
    (< (position ,x seq)
     (position ,y seq))))　
;; 出错的原始是在ｘ的表达式的副作用seq被改变
;; (wrong-before (progn (setq seq '(b a)) 'a) 'b '(a b))　;; NIL 

;; 在一个巨大的let 里求值所有参数 
(defmacro before (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
    (< (position xval seq)
      (position yval seq))))
;; (before (progn (setq seq '(b a)) 'a) 'b '(a b))　;; T 

;; let 技术只能在很有限的一类情况下才可行：
;; a. 所有可能被捕捉的参数都只求值一次，并且
;; b. 没有一个参数需要在宏框架建立的绑定下被求值。


;; （迭代的宏），如果宏调用里面有表达式出现，那么在宏展开后，这些表达式将会在一个新建的绑定中求值。例如在 for 的定义中，循环体必须在一个由宏创建的 do 中进行求值。因此，do 创建的变量绑定会很容易就捕捉到循环里的变量。
;; 我们可以把循环体包在一个闭包里，同时在循环里，不再把直接插入表达式，而只是简单地 funcall 这个闭包。通过这种办法来保护循环中的变量不被捕捉。
(defmacro closure-for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body)) ;; 循环体包在一个闭包里 
      (count ,start (1+ count))
      (limit ,stop))
    ((> count limit)) 
     (funcall b count))) ;; 简单地 funcall 这个闭包, 确保limit不会被传递进body中被求值　

;; 闭包的缺点在于，它们的效率可能不大理想。我们可能会因此造成又一次函数调用

;; 9.5 通过 gensym 避免捕捉

;; 可以给可捕捉的变量取一个怪异的名字，但宏被嵌套调用依旧可能出错
(defmacro strang-name-for ((var start stop) &body body) ; wrong
  '(do ((,var ,start (1+ ,var))
      (xsf2jsh ,stop))
    ((> ,var xsf2jsh))
    ,@body))

(defmacro gensym-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    '(do ((,var ,start (1+ ,var))
        (,gstop ,stop))
      ((> ,var ,gstop))
      ,@body)))

;; 9.6 通过包避免捕捉
;; 如果你从另一个包，比方说 mycode，里调用 for，就算把 limit 作为第一个参数，它也是 mycode::limit 这和 macros::limit 是两回事，后者才是出现在宏框架中的符号。
;; 然而，包还是没能为捕捉问题提供面面俱到的通用解决方案。首先，宏是某些程序不可或缺的组成部分，将它们从自己的包里分离出来会很不方便。其次，这种方法无法为 macros 包里的其他代码提供任何捕捉保护。

;; 9.7 其他名字空间里的捕捉
;; 函数名字的捕捉　
(defun fn (x) (+ x 1))
(defmacro mac (x) `(fn ,x))
(mac 10) ;; 11
(labels ((fn (y) (- y 1)))
  (mac 10)) ;; 9　
;; 通过使用 gensym 作为宏框架局部定义的任何函数的名字

;; 代码块名字（block-name） 同样可以被捕捉，比如说那些被 go 和 throw 使用的标签
;; do 这样的操作符隐式封装在一个名为 nil 的块里。这样在 do 里面的一个 return 或 return-from nil 将从 do 本身而非包含这个 do 的表达式里返回
(block nil
  (list 'a
    (do ((x 1 (1+ x)))
      (nil)
      (if (> x 5)
        (return-from nil x)
        (princ x))))) ;; (A 6)

;; 9.9 为何要庸人自扰
;; 现实应用程序中，对你代码的使用方式做任何假设都是危险的。任何 Lisp 程序都具备现在被称之为 "开放式架构" 的特征。如果你正在写的代码以后会为他人所用，很可能他们调用你代码的方式是出乎你预料的。而且你要担心的不光是人。程序也能编写程序。
;; 即使单个的宏生成的是简单合理的展开式，一旦你开始把宏嵌套着调用，展开式就可能变成巨大的，而且看上去没人能写得出来的程序。在这个前提下，就有必要去预防那些可能使你的宏不正确地展开的情况，就算这种情况像是有意设计出来的。





