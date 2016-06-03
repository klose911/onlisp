;; 12.1 概念 
(setq lst '(a b c))
(setf (car lst) 480)
;; (setf x y) 可以理解成 "务必让 x 的求值结果为 y"
;; 如果第一个参数(在宏展开以后) 是个符号，那么 setf 就只会展开成 setq。但如果第一个参数是个查询语句，那么 setf 则会展开到对应的断言上

(macroexpand-1 '(setf (car lst) 480)) ;; (progn (rplaca lst 480) 480) 
;; 逆变换: 从查询到断言的变换 car与rplaca 
;; car,cdr,nth,aref,get,gethash等都有自己的逆变换

;; 广义变量: 能充当setf第一个参数的表达式!!!!
;; 一个宏调用，只要能展开成可逆引用，那么其本身就一定是可逆的

;; 这样的程序往往更简洁清爽 
(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))
(let ((lst '(a b c)))
  (toggle (car lst))
  lst) ;;  (NIL B C)

(defvar *friends* (make-hash-table))
(setf (gethash 'mary *friends*) (make-hash-table))
*friends* ;;  #S(HASH-TABLE :TEST FASTHASH-EQL (MARY . #S(HASH-TABLE :TEST FASTHASH-EQL))) 
(setf (gethash 'john (gethash 'mary *friends*)) t)
*friends* ;; #S(HASH-TABLE :TEST FASTHASH-EQL (MARY . #S(HASH-TABLE :TEST FASTHASH-EQL (JOHN . T))))
;; 当某人转变立场时，他所有的朋友都变成敌人，而所有的敌人则变成朋友。
(setf (gethash x (gethash y *friends*))
      (not (gethash x (gethash y *friends*))))

(defmacro friend-of (p q)
  `(gethash ,p (gethash ,q *friends*)))
(toggle (friend-of x y))

;; 12.2 多重求值
(defmacro toggle (obj) ; wrong
  '(setf ,obj (not ,obj)))

;; i 将会被增加2次 
(macroexpand-1 '(toggle (nth (incf i) lst))) 
;; (SETF (NTH (INCF I) LST) (NOT (NTH (INCF I) LST)))

(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst) ;; (T NIL T)
;; (setf (nth 0 lst) (not (nth 1 lst)))

;; define-modify-macro宏：被定义宏的宏名，它的附加参数（出现在广义变量之后），以及一个函数名，这个函数将为广义变量产生新值。
(define-modify-macro toggle () not)
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst) ;; (NIL NIL T)

;; 12.3 工具
;;  将同一值赋给多个广义变量 
(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
		       args))))) 
;; 将NIL赋给多个广义变量
(defmacro nilf (&rest args) `(allf nil ,@args))

;; 将t赋给多个广义变量
(defmacro tf (&rest args) `(allf t ,@args)) 

;; 对多个广义变量进行取反操作
(defmacro m-toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
	       args)))
(define-modify-macro toggle2 () not)


;;   (nconc x y) 几乎于 (setq x (nconc x y)), 但是x是NIL的时候，两者效果完全不同
;; (setq x '(1 2))
;; (setq y '(3 4))
;; (nconc x y) -> (1 2 3 4)
;; x -> (1 2 3 4)

;; (setq x nil)
;; (setq y '(3 4))
;; (nconc x y) -> (3 4)
;; x -> nil !!!!!
(define-modify-macro concf (obj) nconc)

(defun conc1f/function (place obj)
  (nconc place (list obj))) 

(define-modify-macro conc1f (obj) conc1f/function) ;; 在列表结尾追加一个元素 

;; (setq x '(1 2))
;; (setq y '(3 4))
;; (conc1f x y) ;; (1 2 (3 4))
;; x ;; (1 2 (3 4)) 

(defun concnew/function (place obj &rest args)
  (unless (apply #'member obj place args)
    (nconc place (list obj)))) 

(define-modify-macro concnew (obj &rest args)
  concnew/function) ;; 只有当这个元素不在列表中时才会追加动作。

;; (setq x '(1 2))
;; (setq y '(3 4)) 
;; (concnew x y) ;; (1 2 (3 4)) 
;; x ;; (1 2 (3 4))
;; (concnew x y) ;; NIL
;; x ;; NIL
;; 如果你正准备通过在结尾处追加元素的方式来构造列表，那么最好用 push ，最后再 nreverse 这个列表
;; 在列表的开头处理数据比在结尾要方便些，因为在结尾处处理数据的话，你首先得先到那里!!!

;; 12.4 更复杂的工具
;; 并非所有基于 setf 的宏都可以用 define-modify-macro

;;(setf (obj-dx o) (* (obj-dx o) factor))
;; (_f * (object-dx o) factor)
;; (macroexpand-1 '(_f * (object-dx o) factor)) -> (SETF (OBJECT-DX O) (* (OBJECT-DX O) FACTOR)) 
(defmacro _f (op place &rest args)   
  `(setf ,place (,op ,place ,@args)))
;; 不幸的是，无法用define-modify-macro正确无误地定义 _f ，因为应用到广义变量上的操作符是由参数给定的!!!

;; (setf arr (make-array 4 :initial-element nil)) -> (NIL NIL NIL NIL)
;; (setf (aref arr 0) 1) -> 1
;; (setf (aref arr 1) 3) -> 3
;; arr -> (1 3 NIL NIL)

(get-setf-expansion '(aref a (incf i)))  
;;  (#:G3485 #:G3486) 临时变量列表 -> vars 
;;   (A (INCF I)) 临时变量赋值 -> forms 
;;   (#:G3487) 另一个临时变量 -> var 
;;   (SYSTEM::STORE #:G3485 #:G3486 #:G3487) 一个赋值的表达式，多半会引用一些内部函数，而这些内部函数不属于 Common Lisp 标准。通常 setf 掩盖了这些函数的存在，但它们必须存在于某处。因为关于它们的所有东西都依赖于具体的实现 -> set
;;   (AREF #:G3485 #:G3486) 将返回广义变量初值的form -> access 

;; (incf (aref a (incf i))) 与下面表达式相同
;; (let* ((#:G3485 A)
;;        (#:G3486 (INCF I))
;;        (#:G3487 (INCF (AREF #:G3485 #:G3486)))) ;; 广义变量的form被包装在INCF调用里 
;;  (SYSTEM::STORE #:G3485 #:G3486 #:G3487))


(defmacro _f (op place &rest args) 
  (multiple-value-bind (vars forms var set access) 
      ;; vars -> 临时变量列表
      ;; forms -> 临时变量赋值
      ;; var -> 返回临时变量
      ;; set -> 保存临时变量
      ;; access ->  返回广义变量初始值的form
      (get-setf-expansion place) 
    `(let* (,@(mapcar #'list vars forms)  ;;临时变量赋值 
	    (,(car var) (,op ,access ,@args))) ;; 临时广义变量的form被包装在_f的参数(op)中 
       ,set))) ;; 保存



(defmacro better-conc1f (lst obj)
  `(_f nconc ,lst (list ,obj)))    

;; (setq x '(1 2))
;; (setq y '(3 4))
;; (better-conc1f x y)
;; x

;; (macroexpand-1 '(_f nconc x y))
;; (LET* ((#:NEW-3539 (NCONC X Y)))
;;       (SETQ X #:NEW-3539))

;; 内置的pushnew的逆操作。
(defmacro pull-1 (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

;; (setq x '(1 2 (a b) 3)) => (1 2 (A B) 3)
;; (pull-1 2 x) => (1 (A B) 3)
;; (pull-1 '(a b) x :test #'equal) => (1 3)
;; (pull-1 4 x) => (1 3)
;; x => (1 3)

;; 注意求值顺序！！！
;; (defmacro pull-wrong (obj seq &rest args) ; wrong
;;   '(setf ,seq (delete ,obj ,seq ,@args)))

;; 修改宏必须将广义变量作为第一个参数，所以我们只得以相反的次序给出前两个参数，这样显得有些不自然。
(define-modify-macro pull-3 (obj &rest args)
  (lambda (seq obj &rest args)
    (apply #'delete obj seq args)))

;; 接受一个初始的函数参数，并且会展开成delete-if而非delete ：
(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	 ,set))))

;; (defmacro popn (n place) 
;;   (multiple-value-bind (vars forms var set access) 
;;                        (get-setf-method place) 
;;     (with-gensyms (gn glst)
;;       `(let* ((,gn ,n)
;;               ,@(mapcar #'list vars forms)
;;               (,glst ,access)
;;               (,(car var) (nthcdr ,gn ,glst)))
;;          (prog1 (subseq ,glst 0 ,gn)
;;                 ,set)))))

;; op: 比较操作符，
;; places: 任意数量的广义变量
;; 不断交换它们的值，直到这些广义变量的顺序符合操作符的要求 
(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
			    (multiple-value-list
			     (get-setf-expansion p)))
			places)) ;; 广义变量展开式列表
	 (temps (apply #'append (mapcar #'third meths)))) ;; 临时广义变量列表
    `(let* ,(mapcar #'list
		    (mapcan #'(lambda (m)
				(append (first m)
					(third m)))
			    meths) ;; 临时变量和临时广义变量列表
		    (mapcan #'(lambda (m)
				(append (second m) ;; 临时变量form
					(list (fifth m)))) ;; 临时广义变量form 
			    meths)) 
       ,@(mapcon #'(lambda (rest)
		     (mapcar
		      #'(lambda (arg)
			  `(unless (,op ,(car rest) ,arg)
			     (rotatef ,(car rest) ,arg)))
		      (cdr rest)))
		 temps) ;; 核心表达式，该表达式生成的代码将被用来对这些临时变量进行冒泡排序
       ,@(mapcar #'fourth meths)))) ;; 存储所有的临时变量

;; (setq x 1 y 2 z 3 ) ;; 3 
;; (sortf > x y z) ;; -> 1 
;; (list x y z) ;; -> (3 2 1)

;; (setq places '(x (incf y) (aref z)))  
;; (multiple-value-list (floor -3 4)) ;; -> (-1 1) 

;; (setq meths (mapcar #'(lambda (p)
;; 			(multiple-value-list
;; 			 (get-setf-expansion p)))
;; 		    places))
;; 列表: 3个广义变量的展开式
;; ((NIL NIL (#:NEW-3458) (SETQ X #:NEW-3458) X) -> x
;;  ((#:TEMP-3462 #:TEMP-3461) (Y (+ Y 1)) (#:NEW-3460) (FUNCALL #'(SETF SETQ) #:NEW-3460 #:TEMP-3462 #:TEMP-3461) (SETQ #:TEMP-3462 #:TEMP-3461)) -> (incf y) 
;;  ((#:G3463) (Z) (#:G3464) (SYSTEM::STORE #:G3463 #:G3464) (AREF #:G3463))) -> (aref z) 

;; 临时广义变量列表
;; (setq temps (apply #'append (mapcar #'third meths)))
;; -> (#:NEW-3458 #:NEW-3460 #:G3464) 

;; (setq sets (mapcar #'fourth meths))
;; 列表：临时广义变量form列表
;;  ((SETQ X #:NEW-3458) 
;;   (FUNCALL #'(SETF SETQ) #:NEW-3460 #:TEMP-3462 #:TEMP-3461)
;;   (SYSTEM::STORE #:G3463 #:G3464))

;; (vars var) 的列表
;; (mapcan #'(lambda (m)
;; 	    (append (first m)
;; 		    (third m)))  
;; 	meths)
;; (#:NEW-3458 #:TEMP-3462 #:TEMP-3461 #:NEW-3460 #:G3463 #:G3464) 

;; (form, access)的列表
;; (mapcan #'(lambda (m)
;; 	    (append (second m)
;; 		    (list (fifth m))))
;; 	meths)
;; (X
;;  Y (+ Y 1) (SETQ #:TEMP-3462 #:TEMP-3461)
;;  Z (AREF #:G3463))

;; 给所有的临时变量初始化
;; (mapcar #'list
;; 	(mapcan #'(lambda (m)
;; 		    (append (first m)
;; 			    (third m)))
;; 		meths)
;; 	(mapcan #'(lambda (m)
;; 		    (append (second m)
;; 			    (list (fifth m))))
;; 		meths)) 

;; ((#:NEW-3458 X)
;;  (#:TEMP-3462 Y) (#:TEMP-3461 (+ Y 1)) (#:NEW-3460 (SETQ #:TEMP-3462 #:TEMP-3461))
;;  (#:G3463 Z) (#:G364 (AREF #:G3463)))

;; (macroexpand-1 '(sortf > x (incf i) (aref z))) 
;; (LET*
;;  ((#:NEW-3533 X) (#:TEMP-3537 I) (#:TEMP-3536 (+ I 1)) (#:NEW-3535 (SETQ #:TEMP-3537 #:TEMP-3536)) (#:G3538 Z)
;;   (#:G3539 (AREF #:G3538))) -> 初始化临时变量
;;  (UNLESS (> #:NEW-3533 #:NEW-3535) (ROTATEF #:NEW-3533 #:NEW-3535))
;;  (UNLESS (> #:NEW-3533 #:G3539) (ROTATEF #:NEW-3533 #:G3539))
;;  (UNLESS (> #:NEW-3535 #:G3539) (ROTATEF #:NEW-3535 #:G3539)) -> bubble sort 
;;  (SETQ X #:NEW-3533) (FUNCALL #'(SETF SETQ) #:NEW-3535 #:TEMP-3537 #:TEMP-3536) (SYSTEM::STORE #:G3538 #:G3539) -> 保存临时变量) 


;; (setq temps '(a b c)) ;; -> (A B C) 
;; (mapcon #'(lambda(rest)
;; 	    (list rest))
;;        temps) ;; -> ((A B C) (B C) (C))  

;; (mapcar
;;  #'(lambda (arg)
;;      `(unless (> ,(car '(a b c)) ,arg)
;; 	(rotatef ,(car '(a b c)) ,arg)))
;;  (cdr '(a b c))) ;; => ((UNLESS (> A B) (ROTATEF A B)) (UNLESS (> A C) (ROTATEF A C)))

;; (mapcar
;;  #'(lambda (arg)
;;      `(unless (> ,(car '(b c)) ,arg)
;; 	(rotatef (car '(b c)) ,arg)))
;;  (cdr '(b c))) ;; => ((UNLESS (> B C) (ROTATEF B C)))

;; (mapcon #'(lambda (rest)
;; 	    (mapcar
;; 	     #'(lambda (arg)
;; 		 `(unless (> ,(car rest) ,arg)
;; 		    (rotatef ,(car rest) ,arg)))
;; 	     (cdr rest)))
;; 	temps)
;;  ((UNLESS (> A B) (ROTATEF A B))
;;   (UNLESS (> A C) (ROTATEF A C))
;;   (UNLESS (> B C) (ROTATEF B C)))


(defmacro _funcall (op place &rest args)
  (let ((g (gensym)))
    (multiple-value-bind (vars forms var set access)
	(get-setf-expansion place)
      `(let* ((,g ,op)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (funcall ,g ,access ,@args)))
	 ,set))))

;; 12.5 逆变换

;; symbole-value -> get方法
;; set -> set方法
(defsetf symbol-value set)

(defun middleguy (x)
  (nth (truncate (1- (list-length x)) 2) x)) 

(defun set-middleguy (x v)
  (unless (null x)
    (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
  v) 

(defsetf middleguy set-middleguy) 

(setq a  '(a b c d)
      b '(x)
      c '(1 2 3 (4 5 6) 7 8 9)) ;; =>  (1 2 3 (4 5 6) 7 8 9)
(setf (middleguy a) 3) ;;  =>  3
(setf (middleguy b) 7) ;; =>  7
(setf (middleguy (middleguy c)) 'middleguy-symbol)  ;; =>  MIDDLEGUY-SYMBOL
a ;; => (A 3 C D)
b ;; => (7)
c ;; => (1 2 3 (4 MIDDLEGUY-SYMBOL 6) 7 8 9)

(defun setf-car (new-car lst)
  (rplaca lst new-car)
  new-car)
;; (defsetf car setf-car) 

(defvar *world* '((a . 2) (b . 16) (c . 50) (d . 20) (f . 12))) 
(defvar *cache* (make-hash-table))

;; 先从缓存查询，查询不到再从ASSOC LIST查询
(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
      (values x y) 
      (cdr (assoc key *world*)))))

;; retrieve的逆操作对象时hashtable的缓存
(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))

(retrieve 'c) ;; => 50
(setf (retrieve 'n) 77) ;; => 77
(retrieve 'n) ;; => 77, T (缓存中找到)
