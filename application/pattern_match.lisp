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

;;(destruc '(?X 'A) 'g #'simple?) ;;=> ((?X (ELT G 0)) ('A (ELT G 1)))

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

;; 第二个参数可以是列表，向量或者它们的任意组合
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
	`(= (length ,pat) ,(length rest))
	`(> (length ,pat) ,(- (length rest) 2)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
	 (cond ((gensym? pat) ;;如果模式参数是一个生成符号，那么它是一个由 destruc 创建用于保存子列表的不可见变量，并且所有我们需要在运行期做的就是测试它是否具有正确的长度
		`(let ((,pat ,expr))
		   (if (and (typep ,pat 'sequence)
			    ,(length-test pat rest))
		       ,then
		       ,else)))
	       ((eq pat '_) then) ;; 通配符不需要做任何的事情
	       ((var? pat) ;; 
		(let ((ge (gensym)))
		  `(let ((,ge ,expr))
		     ;; 模式变量还未赋值或者模式变量已经和表达式中对应的值相同
		     (if (or (gensym? ,pat) (equal ,pat ,ge))
			 ;; 模式变量绑定为表达式的值
			 (let ((,pat ,ge)) ,then)
			 ,else))))
	       (t `(if (equal ,pat ,expr) ;; 一个字面上的值，生成代码去比较它和序列中的对应部分
		       ,then ,else)))))


;; pat: 'A, expr: (ELT G 1)  , rest: NIL
;; pat是常量，调用规则4 
;;(match1 '(('A (ELT G 1))) '(print ?x) nil) 
;; => (IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL)

;; pat: ?X expr: (ELT G 0), rest: NIL
;; pat是模式变量，调用规则3， ?X 绑定到对应的表达式(ELT G 0) 
;; (match1 '((?X (ELT G 0)))
;; 	'(IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL) NIL) 
;; (LET ((#:G3494 (ELT G 0)))
;;   (IF (OR (GENSYM? ?X)
;; 	  (EQUAL ?X #:G3494))
;;       (LET ((?X #:G3494))
;; 	(IF (EQUAL 'A (ELT G 1))
;; 	    (PRINT ?X) NIL)) NIL))

(defmacro with-gensyms-1 (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))
;; (simple? 'a) ;; => T
;; (simple? (quote s)) ;; T 
;; (simple? '(a b)) ;; => NIL

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
	(if (simple? (caar refs))
	    (match1 refs then else)
	    (gen-match (car refs) then else)))))

;; (gen-match '((G SEQ) ((?X (ELT G 0)) ('A (ELT G 1)))) '(print ?X)) ;; 调用点a  
;; 一路递归到列表的结尾，然后在回来的路上构造其返回值

;; (let ((then (gen-match '(((?X (ELT G 0)) ('A (ELT G 1)))) '(print ?X))) ;; 调用点b 
;;     ....

;; (let ((then (gen-match '(('A (ELT G 1)))) '(print ?X))) ;; 调用点c
;;     ....

;; (let ((then (gen-match nil '(print ?X))))
;; gen-match走完所有元素，then就是(print ?x) , 继续执行调用点c

;; (simple? (caar '(('A (ELT G 1)))) => T 
;; (match1 '(('A (ELT G 1))) '(print ?x) nil)
;; => (IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL) 作为返回值then，继续调用点b

;; (gen-match '(((?X (ELT G 0)) ('A (ELT G 1)))) '(IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL)) 
;; (simple? (caar '(((?X (ELT G 0)) ('A (ELT G 1)))))) => NIL
;; (gen-match '((?X (ELT G 0)) '(IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL))
;; (simple? (caar '((?X (ELT G 0))))) => T
;; (match1 '((?X (ELT G 0))) '(IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL) NIL)
;; => (LET ((#:G3494 (ELT G 0)))
;;   (IF (OR (GENSYM? ?X)
;; 	  (EQUAL ?X #:G3494))
;;       (LET ((?X #:G3494))
;; 	(IF (EQUAL 'A (ELT G 1))
;; 	    (PRINT ?X) NIL)) NIL)) 作为返回值，继续调用点a

;; (simple? (caar '((G SEQ) ((?X (ELT G 0)) ('A (ELT G 1)))))) => T
;; (match1 '((G SEQ) ((?X (ELT G 0)) ('A (ELT G 1)))) '(LET ((#:G3494 (ELT G 0)))
;;   (IF (OR (GENSYM? ?X)
;; 	  (EQUAL ?X #:G3494))
;;       (LET ((?X #:G3494))
;; 	(IF (EQUAL 'A (ELT G 1))
;; 	    (PRINT ?X) NIL)) NIL)) 

;; (match1 '(G SEQ)  '(LET ((#:G3494 (ELT G 0)))
;;   (IF (OR (GENSYM? ?X)
;; 	  (EQUAL ?X #:G3494))
;;       (LET ((?X #:G3494))
;; 	(IF (EQUAL 'A (ELT G 1))
;; 	    (PRINT ?X) NIL)) NIL))
;; g是一个生成的symbol，调用规则1, 进行长度校验

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms-1 (gseq gelse)
	`(labels ((,gelse () ,else))
	   ,(gen-match (cons (list gseq seq)
			     (destruc pat gseq #'simple?))
		       then
		       `(,gelse))))))
;; (gen-match  '((G SEQ) ((?X (ELT G 0)) ('A (ELT G 1))))  '(print ?X)) 

(defmacro if-match-fast (pat seq then &optional else)
  ;; 为每个模式变量分配symbol 
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
		 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(setq seq '(b a)) 
(if-match-fast (?x 'a) seq 
	  (print ?x)) ;; => B 
;; (macroexpand-1  '(if-match-fast (?x 'a) seq 
;; 		 (print ?x)))
;; (LET ((?X '#:G3496))
;;   (PAT-MATCH (?X 'A) SEQ
;; 	     (PRINT ?X) NIL))

;; 完整的if-match展开式 
;; (let ((?x '#:g1)) ;; 而变量?x在开始的被绑定到了一个 gensym，以表明它尚未被匹配操作赋给一个值。
;;   (labels ((#:g3 nil nil)) 
;;      match1的规则1：校验模式长度是否和表达式长度一致
;;     (let ((#:g2 seq)) ;;这些变量的名字是用gensym生成的，目的是为了避免捕捉
;;       (if (and (typep #:g2 'sequence)
;;           (= (length #:g2) 2))
;;         match1的规则3：？X未赋值，模式变量赋值为表达式的第一个元素
;;         (let ((#:g5 (elt #:g2 0))) 
;;           (if (or (gensym? ?x) (equal ?x #:g5))
;;             (let ((?x #:g5))
;;              match1的规则4： 校验模式的第二个元素是否与表达式的第二个元素相同
;;               (if (equal 'a (elt #:g2 1))
;;                 (print ?x)
;;                 (#:g3)))
;;             (#:g3)))
;;         (#:g3)))))

;; 在新的 if-match 中，模式元素现在是被求值而不再是被隐式引用了。这意味着Lisp变量可以被用于模式中，和被引用的表达式一样：
(let ((n 3))
  (if-match-fast (?x n 'n '(a b)) '(1 3 n (a b))
		 ?x)) ;; => 1

;; 还有两个进一步的改进，是因为新版本调用了 destruc 而出现。现在模式中可以包含 &rest 或者 &body
;; 并且因为 destruc 使用了一般的序列操作符 elt 和 subseq, 将工作在任何类型的序列上

(defun abab (seq)
  (if-match-fast (?x ?y ?x ?y) seq
    (values ?x ?y)
    nil))

;; 字符串
(abab "abab") ;; => #\a, #\b 
;; 向量 
(abab #(1 2 1 2)) ;; => 1, 2 

(if-match-fast (?x (1 . ?y) . ?x) '((a b) #(1 2 3) a b)
  (values ?x ?y)) ;; => (A B), #(2 3) 
