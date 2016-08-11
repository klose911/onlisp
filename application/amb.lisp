;; 22 非确定性
;; 22.1 概念 
;; choose函数，接受一个列表，并返回一个元素 
;; 如果这个元素被选中，那么这个计算过程就会因为它而导致有一组可能的未来情况与之对应 

;; (let ((x (choose '(1 2 3))))
;;   (if (odd? x)
;;     (+ x 1)
;;     x))  
;; 如果 choose 返回 1，那么这个运算过程将会经过 if 的 then 语句，然后返回 2
;; 如果 choose 返回 2 ，那么这个运算过程将会经过 if 的 else 语句，然后返回 2
;; 如果 choose 返回 3 ，那么这个运算过程将会经过 if 的 then 语句，然后返回 4 

;; 如果将来的可能性中存在某种情况，在这种情况下没有调用 fail ，那么 choose 将可以返回这个选择的结果
;; (let ((x (choose '(1 2))))
;; (if (odd? x)
;;   (fail)
;;   x))
;; 这个choose只会选择2， 这个表达式是确定性的：它总是返回 2  

;; (let ((x (choose '(1 2))))
;;   (if (odd? x)
;;     (let ((y (choose '(a b))))
;;       (if (eq? y 'a)
;;         (fail)
;;         y))
;;     x))
;; 这个表达式总的来说，要么返回 b ，要么返回 2  

;; (let ((x (choose '(1 2))))
;;   (if (odd? x)
;;     (choose '()) ;; 如果要在零个选项里作选择，那么这个 choose 就等价于 fail 
;;     x))

;; 祖上是不是有人名叫 Igor
;; function Ig(n)
;;   if name(n) = 'Igor'
;;     then return n
;;   else if parrents(n)
;;     then return Ig(choose(parents(n))
;;   else fail

;; fail 操作符被用来对 choose 的返回值施加影响
;; 如果我们碰到一个 fail ，那么可以推断 choose 在此之前肯定做了错误的选择!!!

;; 21.4 common lisp实现
;; 变量paths: 保存还没有走过的路径 
(defparameter *paths* nil)
;; 常量failsym: 标识失败，被定义成了符号@ 
(defconstant failsym '@)

;; 当计算过程到达一个有多个可选项的choose表达式
(defmacro choose (&rest choices)
  (if choices
      `(progn
	 ;; 其它选项则会被保存在paths里
	 ,@(mapcar #'(lambda (c)
		       ;; choose把它的参数包装在一个lambda表达式, 形成一个闭包
		       `(push #'(lambda () ,c) *paths*))
		   ;; 注意加入*paths*的顺序
		   (reverse (cdr choices)))
	 ;; 第一个可选项会被求值
	 ,(car choices))
      ;; 没有更多的路径可供重启计算的话，调用fail 
      '(fail)))

;; (macroexpand-1 '(choose (+ x 2) (* x 2) (expt x 2)))
;; (PROGN (PUSH #'(LAMBDA NIL (EXPT X 2)) *PATHS*)
;;        (PUSH #'(LAMBDA NIL (* X 2)) 	  *PATHS*)
;;        (+ X 2))

;; (macroexpand-1 '(choose))
;; (FAIL) 

(defun fail ()
  (if *paths*
      ;; 从保存的选择返回下一个选择，然后再次计算
      (funcall (pop *paths*))
      ;; 如果保存的选择为空，返回一个特殊的failsym(@)标记失败 
      failsym))

;; (defun do2 (x)
;;   (choose (+ x 2) (* x 2) (expt x 2))) 

;; (do2 3) ;; 5 调用 (+ 3 2)
;; *paths* ;; => (#<FUNCTION :LAMBDA NIL (* X 2)> #<FUNCTION :LAMBDA NIL (EXPT X 2)>) 
;; (fail)  ;; 6 调用 (* 3 2)
;; *paths* ;; => (#<FUNCTION :LAMBDA NIL (EXPT X 2)>)
;; (fail)  ;; 9 调用 (expt 3 2)
;; *paths* ;; => nil 
;; (fail)  ;; @ 没有可计算的选择 

(defun cb (fn choices)
  (if choices
      (progn
	(if (cdr choices)
	    (push #'(lambda () (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (fail)))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

;; (choose-bind x '(marrakesh strasbourg vegas)
;;   (format nil "Let's go to ~A." x))
;; => "Let's go to MARRAKESH."

;; (macroexpand-1 '(choose-bind x '(marrakesh strasbourg vegas)
;; 		 (format nil "Let's go to ~A." x)))
;; (CB #'(LAMBDA (X) (FORMAT NIL "Let's go to ~A." X))
;;     '(MARRAKESH STRASBOURG VEGAS))
;; => "Let's go to MARRAKESH."
;; *paths* ;; => (#<FUNCTION :LAMBDA NIL (CB FN (CDR CHOICES))>
;; (fail) ;; => "Let's go to STRASBOURG."
;; *paths* ;; => (#<FUNCTION :LAMBDA NIL (CB FN (CDR CHOICES))>
;; (fail) ;; => "Let's go to VEGAS."
;; *paths* ;; => NIL
;; (fail) ;; => @

(let ((x 2))
  (choose
    (+ x 1)
    (+ x 100)))  ;; 3 
(fail) ;; 102

;; (macroexpand '(let ((x 2))
;;   (choose
;;     (+ x 1)
;;    (+ x 100))))

;; (let ((x 2)) ;; choose和choose-bind作为宏，在它们所在的表达式的词法环境中展开!!! 
;;   (progn
;;     (push #'(lambda () (+ x 100)) 
;;       *paths*) ;; 这两个宏加入paths的是一个闭包，在这个闭包保存了将要用到的待选项，还有被引用到的词法变量的所有绑定!!!
;;     (+ x 1))) 

(=defun two-numbers ()
  ;; 而每个choose-bind 则都被展开成了一个闭包，每个闭包都保存有指向body中引用过的变量的指针，这些变量中包括*cont*
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      ;; 被展开成(funcall *cont* n1 n2) 
      (=values n1 n2))))


;; (setf *cont* #'values)
;; (macroexpand-1 '(choose-bind n1 '(0 1 2 3 4 5)
;; 		 (choose-bind n2 '(0 1 2 3 4 5)
;; 		   (=values n1 n2))))  
;; => (CB #'(LAMBDA (N1) (CHOOSE-BIND N2 '(0 1 2 3 4 5) (=VALUES N1 N2))) '(0 1 2 3 4 5))

;; (macroexpand-1 '(CHOOSE-BIND N2 '(0 1 2 3 4 5) (=VALUES N1 N2)))
;; => (CB #'(LAMBDA (N2) (=VALUES N1 N2)) '(0 1 2 3 4 5))

;; (macroexpand-1 '(=VALUES N1 N2))
;; => (FUNCALL *CONT* N1 N2)

;; (CB #'(LAMBDA (N1)
;; 	(CB #'(LAMBDA (N2)
;; 		(FUNCALL *CONT* N1 N2)) 
;; 		'(0 1 2 3 4 5)))  
;;     '(0 1 2 3 4 5)) 

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
      `(the sum of ,n1 ,n2)
      (fail)))) 

;; (parlor-trick 10) ;; => (THE SUM OF 5 5)

;; (macroexpand-1 '(parlor-trick 10))
;; => (=PARLOR-TRICK *CONT* 10)
;; (macroexpand-1 '(=bind (n1 n2) (two-numbers)
;;     (if (= (+ n1 n2) sum)
;;       `(the sum of ,n1 ,n2)
;;       (fail)))) 

;; (LET ((*CONT*
;;        #'(LAMBDA (N1 N2)
;; 	   (IF (= (+ N1 N2) SUM)
;; 	       `(THE SUM OF ,N1 ,N2)
;; 	       (FAIL)))))
;;   (=TWO-NUMBERS *CONT*))

;; (LET ((*CONT*
;;        #'(LAMBDA (N1 N2)
;; 	   (IF (= (+ N1 N2) 7)
;; 	       `(THE 7 OF ,N1 ,N2)
;; 	       (FAIL)))))
;;   (CB #'(LAMBDA (N1)
;; 	  (CB #'(LAMBDA (N2)
;; 		  (FUNCALL *CONT* N1 N2)) 
;; 	      '(0 1 2 3 4 5)))  
;;       '(0 1 2 3 4 5))) 


;; 一个=values表达式必须出现在choose表达式里面，反过来就行不通
(choose (=values 1) (=values 2)) ;; ok 
;; (=values (choose 1 2)) ; wrong!!!

;; 22.5 剪枝 mark cut 
;; 当知道某一部分的搜索树已经没有价值了，就可以进行一次减枝
;; 1. 必须在树上可以减枝的地方作上标记
;; 2. 进行剪枝

;; 22.6 真正的非确定性 

