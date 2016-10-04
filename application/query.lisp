;; 19 查询编译器
;; 嵌入式语言: 用某种现有的语言实现的, 实现的方式通常是采用转换, 在Lisp里使用宏进行转换
(load "pattern_match.lisp" :external-format charset:utf-8) 

;; 19.1 数据库
;; (painter reynolds joshua english) ;; Joshua Reynolds是一位的英国画家
;; (dates reynolds 1723 1792) ;; Joshua Reynolds生活于1723至1792年

;; 数据库由一张哈希表表示，表项则是一个个事实，事实的谓词作为哈希表的键值
(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))


;; 清空当前数据库 
(defun clear-db (&optional (db *default-db*))
  (clrhash db))

;; 根据谓词来查询哈希表
(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

;; 哈希表的对应key的列表中插入val
(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

;; 给数据库加入新事实。
(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
	  ',args))

(clear-db) 
*default-db* ;; => #S(HASH-TABLE :TEST FASTHASH-EQL)
(fact painter reynolds joshua english) ;; => (REYNOLDS JOSHUA ENGLISH)
*default-db* ;; => #S(HASH-TABLE :TEST FASTHASH-EQL (PAINTER . ((REYNOLDS JOSHUA ENGLISH))))
(fact painter canale antonio venetian) ;; => (CANALE ANTONIO VENETIAN)
*default-db* ;; =>  #S(HASH-TABLE :TEST FASTHASH-EQL (PAINTER . ((CANALE ANTONIO VENETIAN) (REYNOLDS JOSHUA ENGLISH))))
(db-query 'painter) ;; => ((CANALE ANTONIO VENETIAN) (REYNOLDS JOSHUA ENGLISH)), T

;; 19.2 模式匹配查询
;; db-query查询数据库中的数据这种方式不是很灵活
;; 查询语言就是一种用来表达更复杂查询的语言

;; 查询所有出生1697年的画家
;;  (and (painter ?x ?y ?z)
;;     (dates ?x 1697 ?w))

;; 查询语法
;; <query> : (<symbol> <argument>*)
;;         : (not <query>)
;;         : (and <query>*)
;;         : (or <query>*)

;; <argument> : ?<symbol>
;;            : <symbol>
;;            : <number>

;; 对于真值采取怀疑论的观点：除了数据库中存在的记录，其他都认为是假

;; 查询解释器接受查询，并根据它从数据库里生成答案
;; 查询编译器接受查询，然后生成一个程序，当这个程序运行时，从数据库里生成答案

;; 19.3 查询解释器
(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

;; 宏with-answer提供了使用这个查询解释器的清爽简洁的接口方法
;; 第一个参数query是任意合法的查询
;; 其余参数body是一个代码体
(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    ;; dolist not work !!!!
    `(dolist (,binds (interpret-query ',query)) ;; 收集由查询生成的所有绑定的集合
       (let ,(mapcar #'(lambda (v)
			 `(,v (binding ',v ,binds))) ;; 每个绑定集合所指定的变量来迭代整个代码体
		     (vars-in query #'atom)) ;; 当查询成功但却不含有变量时，只求值代码体一次。
	 ,@body))))

;; (vars-in '(or (dates ?x ?y 1772)
;; 	   (dates ?x ?y 1792) #'atom)) ;; (?x ?y)

;; (macroexpand-1 '(with-answer (or (dates ?x ?y 1772)
;;     (dates ?x ?y 1792))
;; 		 (princ (list ?x ?y))))
;; => 
;; (DOLIST (#:G3547 (INTERPRET-QUERY '(OR (DATES ?X ?Y 1772) (DATES ?X ?Y 1792))))
;;   (LET ((?X (BINDING '?X #:G3547))
;; 	(?Y (BINDING '?Y #:G3547)))
;;     (PRINC (LIST ?X ?Y))))

;; (INTERPRET-QUERY '(OR (DATES ?X ?Y 1772) (DATES ?X ?Y 1792))) 
;; => (((?Y . 1697) (?X . HOGARTH)) ((?Y . 1723) (?X . REYNOLDS)))

;; (let ((?X (binding '?X '((?Y . 1697) (?X . HOGARTH))))
;;       (?Y (binding '?Y '((?Y . 1697) (?X . HOGARTH)))))
;;   (PRINC (LIST ?X ?Y)))
;; => (HOGARTH 1697) 

;; 递归地对复杂查询的数据结构进行处理，在这个过程中生成绑定
;; 复杂查询的求值按从左到右的顺序进行
;; 结果会根据逻辑操作符或被滤除，或被组合
(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t (lookup (car expr) (cdr expr) binds))))


;; (interpret-query '(and (painter ?x ?y ?z)
;; 			(dates ?x 1697 ?w)))
;; => (((?W . 1768) (?Z . VENETIAN) (?Y . ANTONIO) (?X . CANALE)) ((?W . 1772) (?Z . ENGLISH) (?Y . WILLIAM) (?X . HOGARTH)))

;; (car '(and (painter ?x ?y ?z)
;;        (dates ?x 1697 ?w)))  ;; => AND
;; (reverse (cdr '(and (painter ?x ?y ?z)
;; 	   (dates ?x 1697 ?w)))) 
;; => ((DATES ?X 1697 ?W) (PAINTER ?X ?Y ?Z))


(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
		  (interpret-query (car clauses) b))
	      (interpret-and (cdr clauses) binds))))

;; (interpret-and
;;  '((DATES ?X 1697 ?W) (PAINTER ?X ?Y ?Z))
;;  '())

(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
	      (interpret-query c binds))
	  clauses))

(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
      nil
      (list binds)))

;; 函数lookup接受一个由谓词及其参数列表所组成的模式
;; 返回一个能够使模式匹配到数据库中某个事实的所有绑定的列表
(defun lookup (pred args &optional binds)
  ;; lookup返回一个含有所有这些列表的列表
  (mapcan #'(lambda (x)
	      ;; 调用match把它们和模式逐一比较。每当匹配成功，就返回一个绑定列表
	      (aif2 (match x args binds) (list it)))
	  ;; 获取所有该谓词的数据库表项 
	  (db-query pred)))

;;(db-query 'painter)
;; => ((CANALE ANTONIO VENETIAN) (REYNOLDS JOSHUA ENGLISH))

;; (match '(REYNOLDS JOSHUA ENGLISH) '(?x ?y english) '((?z 1668)))  
;;   => ((?Y . JOSHUA) (?X . REYNOLDS) (?Z 1668)), T 

;; (mapcan #'(lambda (x)
;; 	    (aif2 (match x '(?x ?y english) '((?z 1668))) (list it)))
;; 	'((CANALE ANTONIO VENETIAN) (REYNOLDS JOSHUA ENGLISH) (ALEX NEWTON ENGLISH)))
;; => (((?Y . JOSHUA) (?X . REYNOLDS) (?Z 1668)) ((?Y . NEWTON) (?X . ALEX) (?Z 1668)))

;; (lookup 'painter '(?x ?y english)) 
;; => (((?Y . JOSHUA) (?X . REYNOLDS)))

;; (with-answer (and (dates ?x ?b ?d)
;;    (lisp (> (- ?d ?b) 70)))
;;   (format t "~A lived over 70 years.~%" ?x))

;; 19.5 查询编译器
;; 解释器实现了我们想要的功能，但效率不好
;; 1. 查询结构在编译期就是已知的，解释器还是把分析工作放在了运行期完成
;; 2. 解释器通过构造列表来保存变量绑定，其实，本可以用变量来保存它们自己的值

;; query中的参数现在可以被求值!!!
(defmacro with-answer-compile (query &body body)
  `(with-gensyms-1 ,(vars-in query #'simple?)
     ;; 宏展开时候调用compile-query函数,产生查询的代码，这些代码在运行期间运行，产生结果
     ,(compile-query query `(progn ,@body))))

;; 这些函数不再生成绑定，它们直接生成代码
(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cadr q) body))
    ;; 增加了一个lisp操作符，它可以跟任意Lisp表达式
    ;; 它不会生成任何绑定，但它将排除那些使表达式返回nil的绑定
    ;; 在需要使用诸如 '>'的内置谓词时，lisp操作符就非常有用
    (lisp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defun compile-simple (q body)
  (let ((fact (gensym)))
    `(dolist (,fact (db-query ',(car q)))
       (pat-match ,(cdr q) ,fact ,body nil))))

(defun compile-and (clauses body)
  (if (null clauses)
      body
      (compile-query (car clauses)
		     (compile-and (cdr clauses) body))))

(defun compile-or (clauses body)
  (if (null clauses)
      nil
      (let ((gbod (gensym))
	    (vars (vars-in body #'simple?)))
	`(labels ((,gbod ,vars ,body))
	   ,@(mapcar #'(lambda (cl)
			 (compile-query cl `(,gbod ,@vars)))
		     clauses)))))

(defun compile-not (q body)
  (let ((tag (gensym)))
    `(if (block ,tag
	   ,(compile-query q `(return-from ,tag nil))
	   t)
	 ,body)))

;; (setq my-favorite-year 1723)
;; (with-answer-compile (dates ?x my-favorite-year ?d) 
;;   (format t "~A was born in my favorite year.~%" ?x))
;; => REYNOLDS was born in my favorite year. 

;; (macroexpand-1 '(with-answer-compile (dates ?x my-favorite-year ?d)
;; 		 (format t "~A was born in my favorite year.~%" ?x)))
;; => 
;; (WITH-GENSYMS-1 (?X ?D)
;;  (DOLIST (#:G3527 (DB-QUERY 'DATES))
;;    (PAT-MATCH (?X MY-FAVORITE-YEAR ?D) #:G3527
;; 	      (PROGN (FORMAT T "~A was born in my favorite year.~%" ?X)) NIL)))


;; (with-gensyms-1 (?x ?d) 
;;   (compile-query '(dates ?x my-favorite-year ?d)
;; 		 '(PROGN (FORMAT T "~A was born in my favorite year.~%" ?X))))
;; (with-gensyms-1 (?x ?d) 
;;   (compile-simple '(dates ?x my-favorite-year ?d)
;; 		  '(PROGN (FORMAT T "~A was born in my favorite year.~%" ?X)))) 


;; (with-gensyms-1 (?x ?d) 
;;   (compile-simple '(dates ?x my-favorite-year ?d)
;; 		  '(PROGN (FORMAT T "~A was born in my favorite year.~%" ?X)))) 
;; => 
;; (DOLIST (#:G3530 (DB-QUERY 'DATES))
;;   (PAT-MATCH (?X MY-FAVORITE-YEAR ?D) #:G3530 (PROGN (FORMAT T "~A was born in my favorite year.~%" ?X)) NIL))

;; (db-query 'dates) ;; => ((REYNOLDS 1723 1792) (CANALE 1697 1768) (HOGARTH 1697 1772))

;; (with-gensyms-1 (?x ?d) 
;;   (PAT-MATCH (?X 1723 ?D)
;; 	     '(REYNOLDS 1723 1792)
;; 	     (PROGN (FORMAT T "~A was born in my favorite year.~%" ?X)) NIL)) 
;; => REYNOLDS was born in my favorite year. ,nil

;; (with-gensyms-1 (?x ?d) 
;;   (PAT-MATCH (?X 1723 ?D)
;; 	     '(CANALE 1697 1768) 
;; 	     (PROGN (FORMAT T "~A was born in my favorite year.~%" ?X)) NIL))
;; => nil, nil

;; (with-answer-compile (and (dates ?x ?b ?d)
;;     (lisp (> (- ?d ?b) 70)))
;;   (format t "~A lived over 70 years.~%" ?x))
;; CANALE lived over 70 years.
;; HOGARTH lived over 70 years.

(with-answer-compile (and (painter ?x _ 'english)
			  (dates ?x ?b _)
			  (not (and (painter ?x2 _ 'venetian)
				    (dates ?x2 ?b _))))
  (princ ?x))
;; => REYNOLDS 
