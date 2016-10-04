;; 18 解构: 是赋值的一般形式
;; 操作符setq和setf的赋值对象只是独立的变量
;; 解构不再只是把单个变量作为第一个参数，而是给出一个关于变量的模式 

;; 18.1 列表上的解构

;; (let ((x (first lst))
;;     (y (second lst))
;;     (z (third lst)))
;;   ...)

;; 解构把赋值和访问操作合二为一
;; x -> (first lst), y->(second lst) z-> (third lst)
;; (destructuring-bind (x y z) lst
;;   ...)

;; 解构使得代码更简短，容易阅读！！！ 
;; (destructuring-bind ((first last) (month day year) . notes)
;;   birthday
;;   ...) 

;; (defun iota (n) (loop for i from 1 to n collect i))
;; a-> 'alpha, b-> 'bee, one->1, two->2, three->3
;; 能够放在宏参数列表的所有变量类型除了(&enviorment)都可以放进解构宏的参数列表内
;; (destructuring-bind ((a &optional (b 'bee)) one two three)
;;      `((alpha) ,@(iota 3)) 
;;    (list a b three two one)) => (ALPHA BEE 3 2 1)

;; 18.2 其他结构: 解构可用于任何对象解构 

;; 18.2.1 通用序列解构操作符
;; 将destruc产生的变量匹配转化成一系列嵌套的let，生成代码 
(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
			 (if (consp (car b))
			     (car b)
			     b))
		     binds)
	 ,(dbind-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body)))) 

;; 遍历匹配模式，将每个变量和运行期对应对象的位置关联在一起
(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
			((eq (car pat) '&rest) (cadr pat))
			((eq (car pat) '&body) (cadr pat))
			(t nil))))
	(if rest
	    `((,rest (subseq ,seq ,n)))
	    (let ((p (car pat))
		  (rec (destruc (cdr pat) seq atom? (1+ n))))
	      (if (funcall atom? p)
		  (cons `(,p (elt ,seq ,n))
			rec)
		  ;; 一个新的变量(生成符号)将被绑定到每个子序列上
		  (let ((var (gensym)))
		    (cons (cons `(,var (elt ,seq ,n))
				(destruc p var atom?))
			  rec))))))))

;; 第二个参数可以是列表，向量或者它们的任意组合
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(dbind (a b c) #(1 2 3)
  (list a b c)) ;; => (1 2 3)

;; (macroexpand-1 '(dbind (a b c) #(1 2 3)
;; 		 (list a b c))) 
;; (LET ((#:G3497 #(1 2 3)))
;;    (LET ((A (ELT #:G3497 0))
;;          (B (ELT #:G3497 1))
;;          (C (ELT #:G3497 2)))
;;       (PROGN (LIST A B C))))

(destruc '(a b c) '#(1 2 3)  #'atom)
;; => ((A (ELT #(1 2 3) 0))
;;     (B (ELT #(1 2 3) 1))
;;     (C (ELT #(1 2 3) 2)))

;; (let ((pat '(a b c))
;;       (seq '#(1 2 3))
;;       (atom? #'atom)
;;       (n 0))    
;;   (let ((p (car pat))
;; 	(rec (destruc (cdr pat) seq atom? (1+ n))))
;;     (if (funcall atom? p)
;; 	(cons `(,p (elt ,seq ,n))
;;               rec)
;; 	(let ((var (gensym)))
;; 	  (cons (cons `(,var (elt ,seq ,n))
;; 		      (destruc p var atom?))
;;                 rec)))))
;; (cons `(c (elt, #(1 2 3) 2)) nil) ;; => ((C (ELT #(1 2 3) 2)))
;; (cons `(b (elt #(1 2 3) 1)) '((C (ELT #(1 2 3) 2)))) ;; => ((B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2)))
;; (cons `(a (elt #(1 2 3) 0)) '((B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2))))
;; ;; => ((A (ELT #(1 2 3) 0)) (B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2)))

(dbind-ex '((A (ELT #(1 2 3) 0)) (B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2))) '((list a b c)))
;; => (LET ((A (ELT #(1 2 3) 0)) (B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2))) (PROGN (LIST A B C)))

;; `(let ,(mapcar #'(lambda (b)
;; 		   (if (consp (car b))
;; 		       (car b)
;; 		       b))
;; 	       '((A (ELT #(1 2 3) 0)) (B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2)))))
;; (LET ((A (ELT #(1 2 3) 0))
;;       (B (ELT #(1 2 3) 1))
;;       (C (ELT #(1 2 3) 2))))

;; `,(dbind-ex (mapcan #'(lambda (b)
;; 			(if (consp (car b))
;; 			    (cdr b)))
;; 		    '((A (ELT #(1 2 3) 0)) (B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2))))
;; 	    '((list a b c)))
;;  (PROGN (LIST A B C))
;; (mapcan #'(lambda (b)
;; 	    (if (consp (car b))
;; 		(cdr b)))
;; 	'((A (ELT #(1 2 3) 0)) (B (ELT #(1 2 3) 1)) (C (ELT #(1 2 3) 2)))) ;; => NIL
;; (consp (car '(A (ELT #(1 2 3) 0)))) ;; => NIL 
;; (consp (car '(B (ELT #(1 2 3) 1)))) ;; => NIL
;; (consp (car '(C (ELT #(1 2 3) 2)))) ;; => NIL
;; `,(dbind-ex nil '((list a b c))) ;; => (PROGN (LIST A B C)) 
;; (progn (list 1 2 3)
;;        (list 3 4 5)) ;; => (3 4 5) 

(dbind (a (b c) d) '(1 #(2 3) 4)
  (list a b c d)) ;; => (1 2 3 4)

;; (macroexpand-1  '(dbind (a (b c) d) '(1 #(2 3) 4)
;; 		  (list a b c d))) 
;; => (LET ((#:G3500 '(1 #(2 3) 4)))
;;      (LET ((A (ELT #:G3500 0)) (#:G3501 (ELT #:G3500 1)) (D (ELT #:G3500 2)))
;;        (LET ((B (ELT #:G3501 0)) (C (ELT #:G3501 1)))
;; 	 (PROGN (LIST A B C D)))))

(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
  (list a b c d)) ;; => (1 #\f "ribble" (2 3 4))
;; (macroexpand-1 '(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
;; 		 (list a b c d)))
;; => (LET ((#:G3504 '(1 "fribble" 2 3 4)))
;;      (LET ((A (ELT #:G3504 0)) (#:G3505 (ELT #:G3504 1)) (D (SUBSEQ #:G3504 2)))
;;        (LET ((B (ELT #:G3505 0)) (C (SUBSEQ #:G3505 1)))
;; 	 (PROGN (LIST A B C D)))))

(destruc '(a (b . c) &rest d) 'seq)
;; ((A (ELT SEQ 0))
;;  ((#:G3506 (ELT SEQ 1))
;;   (B (ELT #:G3506 0))
;;   (C (SUBSEQ #:G3506 1)))
;;  (D (SUBSEQ SEQ 2)))

(dbind-ex (destruc '(a (b . c) &rest d) 'seq) '(body))
;; => (LET ((A (ELT SEQ 0))
;; 	 (#:G3507 (ELT SEQ 1))
;; 	 (D (SUBSEQ SEQ 2)))
;;      (LET ((B (ELT #:G3507 0))
;; 	   (C (SUBSEQ #:G3507 1)))
;;        (PROGN BODY)))

;; 如果运行期给出的序列里没有包含所有期待的元素，解构操作符将产生一个错误：
(dbind (a b c) (list 1 2)) ;; => SYSTEM::LIST-ELT: index 2 too large for (1 2)  

;; 18.2.2 矩阵 with-matrix宏，用于解构两维数组
(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
		  (mapcan
		   #'(lambda (pat)
		       (incf row)
		       (let ((col -1))
			 (mapcar #'(lambda (p)
				     `(,p (aref ,gar
						,row
						,(incf col))))
				 pat)))
		   pats))
	 ,@body))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
			 `(,(car p) (aref ,gar ,@(cdr p))))
		     pat)
	 ,@body)))) 

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))
(setq ar (make-array '(3 3)))
(for (r 0 2)
     (for (c 0 2)
	  (setf (aref ar r c) (+ (* r 10) c))))

(with-matrix ((a b c)
	      (d e f)
	      (g h i)) ar
	      (list a b c d e f g h i)) ;; => (0 1 2 10 11 12 20 21 22)

(with-array ((a 0 0) (d 1 1) (i 2 2)) ar
	    (values a d i)) ;; => 0, 11, 22

;; 18.3 结构体的解构

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; (mkstr pi " pieces of " 'pi) => "3.1415926535897932385L0 pieces of PI"

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
;; (symb 'ar "Madi" #\L #\L 0) => |ARMadiLL0| 

;; name: 结构体名字作为前缀
;; field: 字段名 
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
			 `(,f (,(symb name f) ,gs)))
		     fields)
	 ,@body))))

(defstruct visitor name title firm)
(setq theo (make-visitor :name "Theodebert"
			 :title 'king
			 :firm 'franks))
(with-struct (visitor- name firm title) theo
  (list name firm title)) ;; => ("Theodebert" FRANKS KING) 
;; (macroexpand-1 '(with-struct (visitor- name firm title) theo
;;   (list name firm title))) 
;; => (LET ((#:G3552 THEO))
;;      (LET ((NAME (VISITOR-NAME #:G3552))
;; 	   (FIRM (VISITOR-FIRM #:G3552))
;; 	   (TITLE (VISITOR-TITLE #:G3552)))
;;        (LIST NAME FIRM TITLE)))

;; 18.3 引用的解构 
(defclass thing ()
  ((x :initarg :x :accessor thing-x)
   (y :initarg :y :accessor thing-y)))
;; => #<STANDARD-CLASS THING> 
(setq thing (make-instance 'thing :x 0 :y 1))
;; => #<THING #x000335767B08>
;; with-slots宏中x实际上是thing这个对象的x属性直接引用！！！！
;; incf会直接修改thing中的x的值
(with-slots (x y) thing
  (incf x) (incf y)) 
(thing-x thing) ;; => 1
(thing-y thing) ;; => 2

;; 符号宏 
;; call-by-name/symbol/ref， a是引用了thing中x属性的符号！！！
(symbol-macrolet ((a (thing-x thing)))
  (setf a 100))
(thing-x thing) ;; => 100

;; call-by-value a只是一个局部变量！！！！！
(let ((a (thing-x thing)))
  (setf a 200)) ;; => 200 
(thing-x thing) ;; => 100

;; with-slots宏是借助symbol-macrolet实现的,而不是let
(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
				     (if (consp (car b))
					 (car b)
					 b))
				 binds)
	 ,(wplac-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body))))

(with-places (a b c) #(1 2 3)
	     (list a b c)) ;; => (1 2 3)
(let ((lst '(1 (2 3) 4)))
  (with-places (a (b . c) d) lst
	       (setf a 'uno)
	       (setf c '(tre)))
  lst) ;; => (UNO (2 TRE) 4)

;; dbind将一个变量关联一个值上，而with-places却是将变量关联到一组用来找到一个值的指令集合上, 每一个引用都需要进行一次查询,事实上dbind要快！！！！

;; 18.4 匹配: 解构是赋值的泛化，匹配是解构的泛化

;; (p ?x ?y c ?x)
;; (p a b c a)
;; 当 ?x = a 并且 ?y = b 时匹配

;; (p ?x b ?y a)
;; (p ?y b c a)
;; 当 ?x = ?y = c 时匹配

;; 约定：模式变量是以'？'开始的, 可以通过修改varsym?函数完成

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))
;; (varsym? 'y) => NIL
;; (varsym? '?y1) => T

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form))) 

;; x:模式变量，如果x存在于binds中，返回x的值以及他相关的绑定
(defun binding (x binds)
  (labels ((recbind (x binds)
	     (aif (assoc x binds)
		  (or (recbind (cdr it) binds)
		      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))
;; (binding '?x '((?Y . B) (?X . A))) ;; => A, (?X . A) 
;; (assoc '?x '((?Y . B) (?X . A)))  ;; =>  (?X . A)

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (val (gensym))
	    (win (gensym)))
	`(multiple-value-bind (,val ,win) ,(car cl1)
	   (if (or ,val ,win)
	       (let ((it ,val)) ,@(cdr cl1))
	       (acond2 ,@(cdr clauses)))))))

(defun match (x y &optional binds)
  (acond2
   ;; x于y值相同，_是通配符 第一个返回值是当前的绑定，第二个返回值是T 
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t)) ;; rule 1
   ;; 模式变量x在当前绑定中能找到，校验y是否是绑定中的值，it的值是(binding x binds)的第一个返回值， 就是当前绑定中变量x绑定的值 
   ((binding x binds) (match it y binds)) ;; rule 2 -> rule1 
   ;; 模式变量y在当前绑定中能找到，校验x是否是绑定中的值
   ((binding y binds) (match x it binds)) ;; rule 3 -> rule1
   ;; 模式变量x无法在当前绑定找到对应的值，把x和y建立绑定
   ((varsym? x) (values (cons (cons x y) binds) t)) ;; rule4 -> 执行rule6中(match (cdr x) (cdr y)..
   ;; 模式变量x无法在当前绑定找到对应的值，把x和y建立绑定
   ((varsym? y) (values (cons (cons y x) binds) t)) ;; rule5 -> 执行rule6中(match (cdr x) (cdr y).. 
   ;; 如果(car x) 和 (car y)匹配， 则匹配 (cdr x) (cdr y) 
   ((and (consp x) (consp y) (match (car x) (car y) binds)) 
    (match (cdr x) (cdr y) it)) ;; rule6
   ;; 不匹配
   (t (values nil nil)))) ;; rule7 


(match '(p a b c a) '(p ?x ?y c ?x))
;; => ((?Y . B) (?X . A)), T
;; rule6成立 
(and (consp '(p a b c a)) (consp '(p ?x ?y c ?x)) (match 'p 'p nil)) ;; => NIL,T
;; rule1成立
(match 'p 'p nil) ;; => NIL,T 
(match '(a b c a) '(?x ?y c ?x)) 
;; rule5 成立
(match 'a '?x) ;;  => ((?X . A)), T  
(match '(b c a) '(?y c ?x)
       '((?X . A))) 
;; rule5 成立
(match 'b '?y '((?X . A))) ;; => ((?Y . B) (?X . A)), T 
(match '(c a) '(c ?x) '((?Y . B) (?X . A))) 
;; rule6成立
(match 'c 'c '((?Y . B) (?X . A))) 
;; rule5 成立
(match 'a '?x '((?Y . B) (?X . A)))
;; rule4成立
(binding '?x '((?Y . B) (?X . A))) ;; => A, (?X . A) 
(match 'a '?x '((?Y . B) (?X . A))) ;; => ((?Y . B) (?X . A)), T

;; 慢的匹配操作符
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test  
       (if (or it ,win) ,then ,else))))

;; pat: 模式，模式变量
;; seq: 序列，具体的值
;; then: 模式匹配成功后的执行语句
;; else: 模式匹配失败后的执行语句
(defmacro if-match (pat seq then &optional else)
  ;; 通过比较模式跟序列来建立绑定 
  `(aif2 (match ',pat ,seq)
	 ;; 把then表达是出现的模式变量列表替换成序列中对应绑定的值
	 (let ,(mapcar #'(lambda (v)
			   `(,v (binding ',v it)))
		       (vars-in then #'atom))
	   ,then)
	 ,else))

;; 获取一个表达式中在出现的模式变量列表  
(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))
(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
	    (values ?x ?y)
	    nil))
(abab '(hi ho hi ho)) ;; => HI, HO

;; (match '(?x ?y ?x ?y) '(hi ho hi ho)) ;; => ((?Y . HO) (?X . HI)), T

;; (vars-in '(values ?x ?y)) ;; => (?X ?Y)

;; (mapcar #'(lambda (v)
;; 	    `(,v (binding ',v '((?Y . HO) (?X . HI)))))
;;         (vars-in '(values ?x ?y) #'atom))
;; => ((?X (BINDING '?X '((?Y . HO) (?X . HI)))) (?Y (BINDING '?Y '((?Y . HO) (?X . HI)))))

;; (macroexpand-1 '(if-match (?x ?y ?x ?y) seq
;; 		 (values ?x ?y)
;; 		 nil))
;; => (AIF2 (MATCH '(?X ?Y ?X ?Y) SEQ)
;; 	 (LET ((?X (BINDING '?X IT))
;; 	       (?Y (BINDING '?Y IT)))
;; 	   (VALUES ?X ?Y))
;; 	 NIL)

;; if-match 很短，但并不是非常高效. 在运行期把两个序列都遍历了，尽管第一个序列在编译期就是已知的。更糟糕的是，在进行匹配的过程中，还构造列表来存放变量绑定

;; 快速匹配符
(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

;; 将整个变量列表绑定到 gensym 
(defmacro with-gensyms-1 (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms-1 (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq) 
                             (destruc pat gseq #'simple?))
                       then 
                       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
    (cond ((gensym? pat)
           `(let ((,pat ,expr))
              (if (and (typep ,pat 'sequence)
                       ,(length-test pat rest))
                  ,then
                  ,else)))
          ((eq pat '_) then)
          ((var? pat)
           (let ((ge (gensym)))
             `(let ((,ge ,expr))
                (if (or (gensym? ,pat) (equal ,pat ,ge))
                    (let ((,pat ,ge)) ,then)
                    ,else))))
          (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s) 
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))
