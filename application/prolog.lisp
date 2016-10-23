(load "amb.lisp" :external-format charset:utf-8)
;; (choose-bind x '(0 1 2 3 4 5 6 7 8 9)
;;    (princ x)
;;    (if (= x 6) x (fail)))

(load "query.lisp" :external-format charset:utf-8)
;; (clear-db) 
;; (fact painter reynolds)
;; (fact painter gainsborough)
;; (with-answer (painter ?x)
;;   (print ?x))

;;24 Prolog
;; 规则: 定义了表明事实之间相互蕴含关系，可以根据已有的知识通过推理得到新知识
;; Prolog可以作为嵌入式语言，融合了三个元素：模式匹配，非确定性，规则

;; 24.1 概念
;; 规则
;; If (hungry ?x) and (smells-of ?x turpentine)
;; then (painter ?x)

;; 只要数据库中存在 (hungry raoul) 和 (smells-of raoul turpentine) 这两个事实，那么 ?x = raoul 就能满足查询语句 (painter ?x)，即使数据库中没有 (painter raoul) 这个事实
;; if部分被称为body/条件，then部分是head/结论
;; 试图生成查询的绑定时，程序首先检查规则的head，如果head能满足查询，那么程序会做出响应，为body建立各种绑定
;; 根据定义，如果绑定满足body，那么它也满足head

;; 规则也可以是递归！！！
;; If (surname ?f ?n) and (father ?f ?c)
;; then (surname ?c ?n)

;; 规则:虚拟的事实
;; If (species ?x)and (big ?x) and (fierce ?x)
;; then (rare ?x)
;; 基于”凶猛的大型动物是稀有的“这个规则，可以寻找所有的x，令x满足(species)，(big) 和 (fierce) 这些事实，找到的话就加上一个新的事实(rare)

;; 事实: 规则的退化
;; If true
;; then F
;; 任一事实F的效用，都可以用一个body恒为真的规则来达到

;; 24.2 解释器

;; toplevel接口
;; query: 一个查询语句
;; body:  一组Lisp 表达式
(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)  
     ;; 能返回一组绑定查询结果的代码
     (=bind (binds) (prove-query ',(rep_ query) nil)
       (let ,(mapcar #'(lambda (v)
			 `(,v (fullbind ',v binds)))
		     (vars-in query #'atom))
	 ,@body
	 ;; 使用fail重启下个搜索,非确定性搜索，而不是循环
	 (fail)))))

;; 把_用作规则里的通配符变量
;; 在定义规则的时候，会调用函数rep_，把每个下划线都替换成真正的变量
;; 下划线也可以用在传给with-inference 的查询里面
(defun rep_ (x)
  (if (atom x)
      (if (eq x '_)
	  (gensym "?")
	  x)
      (cons (rep_ (car x))
	    (rep_ (cdr x)))))

;; (rep_ '(painter _)) ;; => (PAINTER #:?3593)

;; pre-defined in pattern_match 
;; (defun varsym? (x)
;;   (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

;; 沿着规则往回跟踪，可以建立一系列绑定, 变量的绑定是其他变量组成的列表
;; 要使用该查询语句的结果，需要一个递归函数来找到相应的绑定
(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
			   (fullbind it b)
			   (gensym)))
	((atom x) x)
	(t (cons (fullbind (car x) b)
		 (fullbind (cdr x) b)))))


;; (setq b '((?x . (?y . ?z)) (?y . foo) (?z . nil)))
;; ;; => ((?X ?Y . ?Z) (?Y . FOO) (?Z)) 

;; (fullbind '?x b) ;; => (FOO) 递归找出?x的值是(FOO) 
;; (values (binding '?x b)) ;; =>   (?Y . ?Z)
;; (varsym? '?x) ;; => T

;; (aif2 (binding '?x b)
;;       (fullbind it b)
;;       (gensym)) ;; => (FOO)
;; (fullbind '(?Y . ?Z)  b)
;; (fullbind '?Y b) ;; =>FOO
;; (values (binding '?Y b)) ;; => FOO 
;; (fullbind '?Z b) ;; => NIL
;; (values (binding '?Z b)) ;; => NIL


;;Prolog解释器则用等价的非确定性搜索来进行匹配
(=defun prove-query (expr binds)
  (case (car expr)
    (and (prove-and (cdr expr) binds))
    (or (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t (prove-simple expr binds))))

(=defun prove-and (clauses binds)
  (if (null clauses)
      (=values binds)
      (=bind (binds) (prove-query (car clauses) binds)
	(prove-and (cdr clauses) binds))))

(=defun prove-or (clauses binds)
  (choose-bind c clauses
    (prove-query c binds)))

;; 查询解释器只需要为(not (painter ?x))建立绑定，如果找到任意的绑定则返回nil
;; 使用非确定性搜索的话，就必须更加小心
;; 不允许(painter ?x)在not的作用域之外fail
;; 同时(painter ?x) 为真时， 也不希望保留其剩下还未探索的路径
;; 所以(painter ?x) 的判断被应用在一个临时的空的搜索路径的环境中。当判断结束时，会恢复原先的路径。 
(=defun prove-not (expr binds)
  (let ((save-paths *paths*))
    (setq *paths* nil) ;; 设置一个临时为空的搜索路径环境
    (choose (=bind (b) (prove-query expr binds)
	      (setq *paths* save-paths)
	      (fail))
	    (progn
	      (setq *paths* save-paths);;搜索结束后,恢复原来的路径
	      (=values binds)))))

;; 让prove-simple为某个查询生成绑定时，
;; 它的非确定地选择一条规则， 并把该规则和查询一同送给implies
(=defun prove-simple (query binds)
  (choose-bind r *rlist* 
    (implies r query binds)))

;; (prove-simple '(painter ?x) nil) 
;; => ((?G3633 . RAOUL) (?G3631 . ?G3633) (?X . ?G3631))

;; 把查询和规则的head匹配起来
;; 一旦匹配成功，implies将调用prove-query，让它帮助为body建立绑定
;; 用这种方法，递归搜索逻辑蕴含树 
(=defun implies (r query binds)
  (let ((r2 (change-vars r)))
       ;; 某条规则的结论(cdr r2)和查询匹配的话，继续匹配这条规则的条件(car r2), 直到找到某条事实(弱化的规则)匹配查询
    (aif2 (match query (cdr r2) binds)
	  (prove-query (car r2) it)
	  ;; 这条规则无法匹配事实，则继续进行下条规则匹配
	  (fail))))

;; (car *rlist*) ;; =>  ((AND (HUNGRY ?X)  (SMELLS-OF ?X TURPENTINE)) PAINTER ?X)
;; (cdr (change-vars (car *rlist*))) ;; => (PAINTER ?G3661)
;; (car (change-vars (car *rlist*))) ;; => (AND (HUNGRY ?G3662) (SMELLS-OF ?G3662 TURPENTINE))
;; (match '(painter ?x) '(PAINTER ?G3661) nil) => ((?X . ?G3661)), T 
;; (prove-query '(AND (HUNGRY ?G3662) (SMELLS-OF ?G3662 TURPENTINE)) '((?X . ?G3661)))
;; => ((?G3663 . RAOUL) (?G3661 . ?G3663) (?X . ?G3661))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args)))) 
;; (symb '? (gensym))
;; => ?G3628 

;; 把规则中所有的变量换成新生成的
;; 如果在某个规则里使用了?x，那么这个?x是和其它规则里面的?x是没有关系的
;; 为了避免现有绑定之间发生冲突，每应用一条规则，都会调用change-vars 。
(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
		      (cons v (symb '? (gensym))))
		  (vars-in r #'atom))
	  r))
;; (sublis '((x . 100) (z . zprime))
;; 	;; 替换ｘ和ｚ
;; 	'(plus x (minus g z x p) 4 . x))
;; => (PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100)

;; (change-vars '(?x ?u ?v))
;; => (?G3630 ?G3631 ?G3632)

;; 规则定义

;; ⟨rule⟩ : (<- ⟨sentence⟩ ⟨query⟩)

;; ⟨query⟩ : (not ⟨query⟩)
;;         : (and ⟨query⟩*)
;;         : (or ⟨query⟩*)
;;         : ⟨sentence⟩

;; ⟨sentence⟩ : (⟨symbol⟩ ⟨argument⟩*)

;; ⟨argument⟩ : ⟨variable⟩
;;            : ⟨symbol⟩
;;            : ⟨number⟩

;; ⟨variable⟩ : ?⟨symbol⟩


;; 规则被放在全局列表 *rlist* 中
;; 每个规则由 body 和 head 所组成的点对(dottedpair)表达
;; 当一个规则被定义后，任一下划线会被替换为一个唯一的变量
(defvar *rlist* nil)

(define-modify-macro conc1f (obj) 
  (lambda (place obj)
    (nconc place (list obj))))


;; <- 的定义遵循了三个惯例
;; 加入新规则的时候，应当把规则放到列表末尾，而不是最前面，这样应用规则时的顺序就和定义规则的顺序一致了
;; 在表示规则的时候，要把head 放在前面，因为程序查看规则的顺序就是如此
;; 如果body里有多个表达式的话，它们事实上被放在了看不见的and里面
(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
		 (car ant)
		 `(and ,@ant))))
    ;; 在 <- 的展开式最外层调用了 length ，其目的是为了避免在 toplevel 调用 <- 时，打印出巨大的列表
    `(length (conc1f *rlist* (rep_ (cons ',ant ',con)))))) 


;; 24.3 定义规则和事实

;; 定义规则
(<- (painter ?x) (and (hungry ?x)
		      (smells-of ?x turpentine))) 
;; *rlist* ;; => (((AND (HUNGRY ?X) (SMELLS-OF ?X TURPENTINE)) PAINTER ?X))

;; (macroexpand-1 '(<- (painter ?x) (and (hungry ?x)
;; 				  (smells-of ?x turpentine))))
;; => (LENGTH (CONC1F
;; 	    *RLIST*
;; 	    (REP_ (CONS '(AND (HUNGRY ?X) (SMELLS-OF ?X TURPENTINE))
;; 			'(PAINTER ?X))))) 

(<- (hungry ?x) (or (gaunt ?x) (eats-ravenously ?x)))
;; *rlist*
;; => (((AND (HUNGRY ?X) (SMELLS-OF ?X TURPENTINE)) PAINTER ?X)
;;     ((OR (GAUNT ?X) (EATS-RAVENOUSLY ?X)) HUNGRY ?X)) 

;; 定义事实
(<- (gaunt raoul))
(<- (smells-of raoul turpentine))
(<- (painter rubens))

;; (with-inference (painter ?x)
;;   (print ?x)) 
;; RAOUL 
;; RUBENS

;; (macroexpand-1 '(with-inference (painter ?x)
;; 		 (print ?x)))
;; => (PROGN (SETQ *PATHS* NIL)
;;        (=BIND (BINDS)
;; 	   (PROVE-QUERY '(PAINTER ?X) NIL)
;; 	 (LET ((?X (FULLBIND '?X BINDS)))
;; 	   (PRINT ?X) (FAIL))))

;; (macroexpand-1 '(prove-query '(painter ?x) nil))  
;; => (=PROVE-QUERY *CONT* '(PAINTER ?X) NIL)

;; (FULLBIND '?X '((?G3633 . RAOUL) (?G3631 . ?G3633) (?X . ?G3631))) ;; => ROUL 
;; (FAIL) ;; => ((?X . RUBENS))
;; (FAIL) ;; => @

;; 规则: 对所有可能的绑定，都可以令给定形式的事实为真
(<- (eats ?x ?f) (glutton ?x))
(<- (glutton hubert))
;; (with-inference (eats ?x spinach)
;;   (print ?x))
;; HUBERT
;; @

;; (with-inference (eats ?x ?y)
;;   (print (list ?x ?y)))
;; (HUBERT #:G3639)
;; @

;; 指定一个特定形式的事实对任意参数都为真
(<- (identical ?x ?x))  
;; (with-inference (identical a ?x)
;;   (print ?x))
;; A
;; @

;; 我们的语法和传统的Prolog 语法间有如下几点区别：
;; 1. 使用以问号开头的符号，而非大写字母来表示变量, 因为Common Lisp 缺省是不区分大小写的，用大写字母的话可能会得不偿失
;; 2. [ ]变成了nil
;; 3. 形如 [x | y] 的表达式成了(x . y)
;; 4. 形如 [x，y，...] 的表达式成了(x y...)
;; 5. 断言被挪到了括弧里面，而且用来分隔参数的逗号也被去掉了： ( ，，...) 成了 ( ...) 

(<- (append nil ?xs ?xs))
(<- (append (?x . ?xs) ?ys (?x . ?zs))
    (append ?xs ?ys ?zs))

;; (with-inference (append ?x (c d) (a b c d))
;;   (format t "Left: ~A~%" ?x))
;; A Left: (A B)
;; => @

;; (with-inference (append (a b) ?x (a b c d))
;;   (format t "Right: ~A~%" ?x))
;; Right: (C D)
;; => @

;; (with-inference (append (a b) (c d) ?x)
;;   (format t "Whole: ~A~%" ?x))
;; Whole: (A B C D)
;; => @

;; prolog规则可以推导出所有可能的组合答案
 (with-inference (append ?x ?y (a b c))
   (format t "Left: ~A Right: ~A~%" ?x ?y))
;; Left: NIL Right: (A B C)
;; Left: (A) Right: (B C)
;; Left: (A B) Right: (C)
;; Left: (A B C) Right: NIL
;; => @

;; 24.5 编译器
