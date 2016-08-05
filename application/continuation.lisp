;; 20 延续: 在运行中被暂停了的程序, 含有计算状态的单个函数型对象
;; 当这个对象被求值时，就会在它上次停下来的地方重新启动之前保存下来的计算
;; 表示挂起的进程, 表示搜索树中的节点 

;; 20.1 schem中的延续: 一个代表着计算的将来的函数

;; 把续延视为带一个参数的函数, 接下来的计算可以通过在返回值上调用这个函数来重现
;; (/ (- x 1) 2)中当求值 (- x 1) 时，外面的 / 表达式就在等着这个值， 以此类推，最后总是回到toplevel, print正等在那里
;; 表达式 (- x 1) 被求值时，续延将是：(lambda (val) (/ val 2))  

;; (define (f1 w)
;;   (let ((y (f2 w)))
;;     (if (integer? y) (list 'a y) 'b)))

;; (define (f2 x)
;;   (/ (- x 1) 2)) 
;; 表达式 (- x 1) 被求值时，续延将是:
;; (lambda (val)
;;   (let ((y (/ val 2)))
;;     (if (integer? y) (list 'a y) 'b)))

;; 在Scheme中，续延和函数同样是第一类对象
;; 你可以要求 Scheme 返回当前的续延，然后它将为你生成一个只有单个参数的函数，以表示未来的计算
;; 可以在任意长时间地保存这个对象，然后在你调用它时，将重启当它被创建时所发生的计算

;; 续延可以理解成是一种广义的闭包。闭包就是一个函数加上一些指向闭包创建时可见的词法变量的指针
;; 续延则是一个函数加上一个指向其创建时所在的整个栈的指针
;; 当续延被求值时，它返回的是使用自己的栈拷贝算出的结果，而没有用当前栈
;; 如果某个续延是在 T1 时刻创建的，而在 T2 时刻被求值，那么它求值时使用的将是 T1 时刻的栈!!!!! 

;; 20.2  续延传递宏
;; Scheme 的续延给了我们两样东西
;; 1: 续延被创建时所有变量的绑定 -- 在一个词法作用域的 Lisp 里，闭包可以实现！！！
;; 2: 计算的状态 从那时起将要发生什么 -- 闭包也可以实现，把计算的状态同样也保存在变量绑定里！！！ 

;; *cont* 将被绑定到当前的续延
(setq *cont* #'identity) 

;; =defun定义了一个函数和一个宏，这个宏会展开成对该函数的调用
;; 宏定义必须在先，原因是被定义的函数有可能会调用自己
;; 实际被调用的不是函数而是宏
(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms)) 
       (defun ,f (*cont* ,@parms) ,@body)))) 


;; =values的定义显示了这个续延的用场
;; =values 把retvals回传*cont* 
(defmacro =values (&rest retvals)
  ;; 调用当前的*cont*
  `(funcall *cont* ,@retvals)) 
;;(macroexpand-1 '(=values (1+ n))) 
;; (FUNCALL *CONT* (1+ N)) 

(=defun add1 (x) 
  (=values (1+ x))) 
;; => =ADD1 

;; (macroexpand-1 '(=defun add1 (x) (=values (1+ x)))) 
;; (PROGN 
;;     (DEFMACRO ADD1 (X) ;; 定义了add1宏
;;       (LIST '=ADD1 '*CONT* X))  ;; (add1 x) 会被展开成(=add1 *cont* x) 宏展开时的*cont*可能是全局变量，也可能是bind定义的局部代码体
;;     (DEFUN =ADD1 (*CONT* X) ;; 定义了=add1函数, 这个函数会带上参数*cont*, 告诉那个由=defun定义的函数对其返回值做什么。
;;       (=VALUES (1+ X)))) ;; =add1函数通过=values来返回值

;; (add1 2)
;; => 3

;; (macroexpand '(add1 2)) ;;  (=ADD1 *CONT* 2)  
;; (funcall #'(lambda (*cont* n) (=values (1+ n))) *cont* 2) 

;; (macroexpand-1 '(=values (1+ n))) 
;; => (FUNCALL *CONT* (1+ N)) ;; 当=values被宏展开时，它将捕捉 *cont*，并用它模拟从函数返回值的过程


;; (funcall #'(lambda (*cont* n) 
;; 	     (funcall *cont* (1+ n))) 
;; 	 *cont* 2) ;; 这里的*cont*是最初定义的#<SYSTEM-FUNCTION IDENTITY>
;; => 3 


;; 如果有相同数量参数的=bind等着=values的话，它可以返回多值
;; 参数列表params，表达式expr，以及一个代码体body
;; =bind 把代码体body绑定到局部*cont*，然后执行表达式expr 
(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ;; =bind的展开式会创建一个称为 *cont* 的新变量
     ,expr)) ;; 参数将被绑定到表达式返回的值上，而代码体在这些绑定下被求值


(=defun message ()
  ;; 把'hello 'there作用于当前的*cont* 
  (=values 'hello 'there)) 

;;#'=message 
;; #<FUNCTION =MESSAGE (*CONT*) (DECLARE (SYSTEM::IN-DEFUN =MESSAGE)) (BLOCK =MESSAGE (=VALUES 'HELLO 'THERE))>  
;; (macroexpand-1 '(=values 'hello 'there)) 
;; (FUNCALL *CONT* 'HELLO 'THERE)  

(=defun baz ()
  ;;把代码体(list m n)绑定到新创建的局部变量*cont*, 这个局部变量会被message捕获，并作用于'hello 'there 
  (=bind (m n) (message)
    (=values (list m n)))) 

;;#'=baz
;;#<FUNCTION =BAZ (*CONT*) (DECLARE (SYSTEM::IN-DEFUN =BAZ))
;;     (BLOCK =BAZ (=BIND (M N) (MESSAGE) (=VALUES (LIST M N)))) 

;; (macroexpand-1 '(=BIND (M N) (MESSAGE) (=VALUES (LIST M N))))  
;; =bind 的展开式会创建一个称为 *cont* 的新变量。baz 的主体展开成：
;;(LET ((*CONT* #'(LAMBDA (M N) 
;; 		  (=VALUES (LIST M N)))))
;;  (MESSAGE)) 

;; (macroexpand-1 '(=values (list m n))) 
;;(FUNCALL *CONT* (LIST M N)) 

;; (macroexpand-1 '(message)) ;; 
;;  (=MESSAGE *CONT*) 

;; (let ((*cont* #'(lambda (m n) ;; 创建新的局部变量*cont*, 然后传递给=message函数 
;; 		  (funcall *cont* (list m n))))) ;; 这里的*cont*是调用=baz时候的全部变量*cont*( #<SYSTEM-FUNCTION IDENTITY>)
;;   (=message *cont*)) 
;; => (HELLO THERE) 

;; (let ((*cont* #'(lambda (m n) 
;; 		  (funcall *cont* (list m n))))) ;; baz代码体
;;   ;; =message函数调用会利用新创建的*cont* 来 "返回" ，结果就是去求值baz的代码体
;;   (funcall *cont* 'hello 'there))  
;;  => (HELLO THERE) 

;; (funcall #'(lambda (m n) ;; baz中的代码体被传入message
;; 	     (funcall *cont* (list m n))) ;; baz调用时候的*cont*依旧作为调用完baz代码体的返回  
;; 	 'hello 'there) ;; message的参数被绑定到baz的代码体
;;  => (HELLO THERE) 

;; 每个*cont* 的绑定都包含了上一个*cont* 绑定的闭包，它们串成一条锁链，锁链的尽头指向那个全局的值。
;; 当代码的主体求值到一个=values时，调用当前的*cont*就能够返回到最初的主调函数那里

(defmacro =lambda (parms &body body) 
  `#'(lambda (*cont* ,@parms) ,@body))
;; (macroexpand-1 '(=lambda (x y) (+ x y)))  
;; => #'(LAMBDA (*CONT* X Y) (+ X Y))  

;; =funcall和=apply适用于由=lambda 定义的函数
;; 注意那些用=defun 定义出来的"函数"，因为它们的真实身份是宏，所以不能作为参数传给apply或funcall
(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args)) 

;; 把(fn 9) 绑定到y， 然后再调用(format nil "9 + 1 = ~A" y)
;; (let ((fn (=lambda (n) (add1 n))))
;;   (=bind (y) (=funcall fn 9) 
;;     (format nil "9 + 1 = ~A" y))) ;; => "9 + 1 = 10"

;; (macroexpand-1 '(=bind (y) (=funcall fn 9)
;; 		 (format nil "9 + 1 = ~A" y)))
;; (LET ((*CONT* #'(LAMBDA (Y) (FORMAT NIL "9 + 1 = ~A" Y))))
;;   (=FUNCALL FN 9))
;; (macroexpand-1 '(=funcall (=lambda (n) (add1 n)) 9)) 
;; (FUNCALL (=LAMBDA (N) (ADD1 N))
;; 	 *CONT* 9)

;; (LET ((*CONT* #'(LAMBDA (Y) (FORMAT NIL "9 + 1 = ~A" Y))))
;;   (FUNCALL (=LAMBDA (N) (ADD1 N))
;; 	   *CONT* 9) )

;; (macroexpand-1 '(=lambda (n) (add1 n))) 
;; #'(LAMBDA (*CONT* N) (ADD1 N))

;; (FUNCALL #'(LAMBDA (*CONT* N) (ADD1 N))
;; 	 #'(LAMBDA (Y) (FORMAT NIL "9 + 1 = ~A" Y)) 9)

;; 延续宏的限制
;; 一个用=defun定义的函数的参数列表必须完全由参数名组成。
;; 使用续延，或者调用其他做这件事的函数的函数，必须用=lambda 或=defun 来定义。
;; 这些函数必须终结于用=values 来返回值，或者调用其他遵守该约束的函数。
;; 如果一个=bind ，=values ，或者=funcall 表达式出现在一段代码里，它必须是一个尾调用。任何在=bind之后求值的代码必须放在其代码体里。所以如果我们想要依次有几个=bind ，它们必须被嵌套：

;; (=defun foo (x)
;;   (=bind (y) (bar x)
;;     (format t "Ho ")
;;     (=bind (z) (baz x)
;;       (format t "Hum.")
;;       (=values x y z))))

(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))

(setq t1 '(a (b (d h)) (c e (f i) g))
      t2 '(1 (2 (3 6 7) 4 5)))

;;(dft t1)
;; => (A (B (D H)) (C E (F I) G))
;; => NIL

;; *saved*用栈的结构来保存续延
(defvar *saved* nil)


(=defun re-start ()
  (if *saved*
      ;; 压出栈顶的续延，继续执行  
      (funcall (pop *saved*)) 
      (=values 'done)))

;; 对于每个节点返回相应的
(=defun dft-node (tree) 
  (cond ((null tree) (re-start)) ;; 当前分支到头，执行上个续延
	((atom tree) (=values tree)) ;; 根节点，延时返回当前节点值
	(t (push #'(lambda () (dft-node (cdr tree)))
		 *saved*) ;; 遍历cdr压入栈
	   (dft-node (car tree))))) ;; 遍历car

;; 对当前的续延进行求值
(=defun dft2 (tree)
  (setq *saved* nil) ;; 清空栈
  (=bind (node) (dft-node tree) ;; 绑定了dft-node中调用(=values tree)的*cont* 
    (cond ((eq node 'done) (=values nil)) ;; 这段代码在=values会被执行 
	  (t (princ node) 
	     (re-start)))))
;;(dft2 t1)
;; => ABDHCEFIG
;; => NIL 

;; (macroexpand-1  '(=bind (node) (dft-node tree) ;; 
;; 		  (cond ((eq node 'done) (=values nil))
;; 			(t (princ node)
;; 			   (re-start))))) 

;; (LET ((*CONT* #'(LAMBDA (NODE)
;; 		  (COND ((EQ NODE 'DONE) (=VALUES NIL))
;; 			(T (PRINC NODE) (RE-START))))))
;;   (DFT-NODE TREE))
;;(macroexpand-1  '(dft-node tree)) ;;
;;(=DFT-NODE *CONT* TREE)

;; #'=dft-node
;; #<FUNCTION =DFT-NODE (*CONT* TREE) (DECLARE (SYSTEM::IN-DEFUN =DFT-NODE))
;; (BLOCK =DFT-NODE
;;   (COND ((NULL TREE) (RE-START)) ((ATOM TREE) (=VALUES TREE))
;; 	(T (PUSH #'(LAMBDA NIL (DFT-NODE (CDR TREE))) *SAVED*) (DFT-NODE (CAR TREE)))))

;;(macroexpand-1  '(=values tree)) ;;
;;(FUNCALL *CONT* TREE)

;; (dft-node '(a (b (d h)) (c e (f i) g)))
;; 把 #'(lambda nil (dft-node '((b (d h)) (c e (f i) g))) 保存到*saved*
;; 执行(dft-node 'a)
;; 执行(=values 'a)
;; 执行 (funcall #'(LAMBDA (NODE)
;;  		  (COND ((EQ NODE 'DONE) (=VALUES NIL))
;;  			(T (PRINC NODE) (RE-START)))) 'a)
;; 打印 a 执行RE-START

;; 从*saved*取出，执行 (dft-node '((b (d h)) (c e (f i) g)))
;; 把 #'(lambda nil (dft-node '((c e (f i) g))) 保存到*saved*
;; 执行 (dft-node '(b (d h)))
;; 把#'(lambda nil (dft-node '((d h))) 保存到*saved*

;; 执行 (dft-node 'b)
;; 执行(=values 'b)
;; 执行 (funcall #'(LAMBDA (NODE)
;;  		  (COND ((EQ NODE 'DONE) (=VALUES NIL))
;;  			(T (PRINC NODE) (RE-START)))) 'b)
;; 打印 b 执行RE-START
;; 执行(dft-node '((d h)))
;; 把#'(lambda nil (dft-node nil) 保存到*saved*
;; 执行(dft-node '(d h))
;; 把#'(lambda nil (dft-node '(h)) 保存到*saved*
;; 执行(dft-node 'd)
;; 执行(=values 'd)
;; 执行 (funcall #'(LAMBDA (NODE)
;;  		  (COND ((EQ NODE 'DONE) (=VALUES NIL))
;;  			(T (PRINC NODE) (RE-START)))) 'd)
;; 打印d 执行RESTART 
;; 执行(dft-node '(h))
;; 把#'(lambda nil (dft-node nil) 保存到*saved*
;; 执行(dft-node 'h)
;; 执行(=values 'h)
;; 执行 (funcall #'(LAMBDA (NODE)
;;  		  (COND ((EQ NODE 'DONE) (=VALUES NIL))
;;  			(T (PRINC NODE) (RE-START)))) 'h)
;; 打印h 执行RESTART
;; 执行 (dft-node nil)
;; 执行RESTART
;; 执行 (dft-node nil)
;; 执行RESTART
;; (dft-node '((c e (f i) g)))
;; ........直到g被打印完毕, *saved*重新空
;; 执行RESTART
;; 执行(=VALUSE 'DONE)
;; 执行 (funcall #'(LAMBDA (NODE)
;;  		  (COND ((EQ NODE 'DONE) (=VALUES NIL))
;;  			(T (PRINC NODE) (RE-START)))) 'DONE)
;; 执行 (=VALUES NIL)
;; 执行(FUNCALL *CONT* NIL) 这里的*CONT*是调用dft2时候的， 也就是#’identity ！！！
;; 返回toplevel 

;; 20.3 CPS
(defun rev (x)
  (if (null x)
    nil
    (append (rev (cdr x)) (list (car x)))))

;; 在第一次递归时，续延是 identity；此时函数的任务就是向toplevel返回其当前的值
(defun rev2 (x)
  (revc x #'identity))

;; 函数得到了一个附加的形参(这里是k)，其值将是当前的续延。这个续延是个闭包，它代表了对函数的当前值应该做些什么
(defun revc (x k)
  (if (null x)
    (funcall k nil)
    (revc (cdr x)
      #'(lambda (w)
	  (funcall k (append w (list (car x))))))))

;; 在第二次递归时，续延将等价于
#'(lambda (w)
    (identity (append w (list (car x))))) ;; (cdr x) 是作为w的实参传入

;; 为了做 CPS 转换，我们需要 code-walker，它是一种能够遍历程序源代码树的程序
;; 为 Common Lisp 编写 code-walker 并非易事。要真正能有用，code-walker 的功能不能仅限于简单地遍历表达式。它还需要相当了解表达式的作用
;; CPS实际上比续延更强大！！！
