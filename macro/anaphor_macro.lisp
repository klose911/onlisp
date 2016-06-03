;; 14 指代宏  指代(anaphor) 是一种引用对话中曾提及事物的表达方式
;; 14.1 指代的种种变形
;; 代词，实际上是一种可捕捉的符号。我们可以通过指定某些符号，让它们充当代词，然后再编写宏有意地捕捉这些符号，用这种方式来使用代词

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))
(macroexpand-1 '(aif (big-long-calculation)
		 (foo it))) ;; (aif (big-long-calculation) (foo it))  在宏调用中，it 看起来是自由的，
;; (LET ((IT (BIG-LONG-CALCULATION))) 但事实上在 aif 展开时，表达式 (foo it) 会被插入到一个上下文中，而 it 的绑定就位于该上下文
;;   (IF IT (FOO IT) NIL))

;; when的指代版本 
(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))
(macroexpand-1 '(awhen (big-long-calculation)
		 (foo it)
		 (bar it)))
;; (AIF (BIG-LONG-CALCULATION)
;;      (PROGN (FOO IT)
;; 	    (BAR IT)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

;; 如果一个程序需要等待(poll) 某个外部数据源的话，类似 while 和 awhile 这样的宏就可以派上用场了
;; 而且，如果你在等待一个数据源，除非你想做的仅是静待它改变状态，否则你肯定会想用从数据源那里获得的数据做些什么
(macroexpand-1 '(awhile (poll *fridge*)
		 (eat it)))
;; (DO ((IT (POLL *FRIDGE*) (POLL *FRIDGE*)))
;;     ((NOT IT)) (EAT IT))


;; and 的指代版本；每次求值它的实参，it 都将被绑定到前一个参数返回的值上
;; 在实践中，aand 倾向于在那些做条件查询的程序
(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))
;; 如果一个宏总是产生包含对其自身调用的展开式，那么展开过程将永不终止。虽然 aand 是递归的，但是它却没有这个问题，因为在基本情形里它的展开式没有引用 aand !!!!

(macroexpand-1 '(aand (owner x)
		 (address it)
		 (town it)))
;; (AIF (OWNER X)
;;      (AAND (ADDRESS IT) (TOWN IT)))
;; (let ((own (owner x)))
;;   (if own
;;     (let ((adr (address own)))
;;       (if adr (town adr)))))


(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym
	       (let ((it ,sym)) ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))
