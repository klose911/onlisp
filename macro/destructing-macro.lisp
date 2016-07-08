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
