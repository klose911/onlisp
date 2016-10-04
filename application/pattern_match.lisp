;; 获取一个表达式中在出现的模式变量列表  
(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun gensym? (s) 
  (and (symbolp s) (not (symbol-package s))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form))) 

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test  
       (if (or it ,win) ,then ,else))))

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

;; 将整个变量列表绑定到 gensym 
(defmacro with-gensyms-1 (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

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

;; 第二个参数可以是列表，向量或者它们的任意组合
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

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

(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms-1 (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq) 
                             (destruc pat gseq #'simple?))
                       then 
                       `(,gelse))))))

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

;; (if-match (?x (1 . ?y) . ?x) '((a b) #(1 2 3) a b)
;;   (values ?x ?y)) 
