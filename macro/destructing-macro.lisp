;; 18 �⹹: �Ǹ�ֵ��һ����ʽ
;; ������setq��setf�ĸ�ֵ����ֻ�Ƕ����ı���
;; �⹹����ֻ�ǰѵ���������Ϊ��һ�����������Ǹ���һ�����ڱ�����ģʽ 

;; 18.1 �б��ϵĽ⹹

;; (let ((x (first lst))
;;     (y (second lst))
;;     (z (third lst)))
;;   ...)

;; �⹹�Ѹ�ֵ�ͷ��ʲ����϶�Ϊһ
;; x -> (first lst), y->(second lst) z-> (third lst)
;; (destructuring-bind (x y z) lst
;;   ...)

;; �⹹ʹ�ô������̣������Ķ������� 
;; (destructuring-bind ((first last) (month day year) . notes)
;;   birthday
;;   ...) 

;; (defun iota (n) (loop for i from 1 to n collect i))
;; a-> 'alpha, b-> 'bee, one->1, two->2, three->3
;; �ܹ����ں�����б�����б������ͳ���(&enviorment)�����ԷŽ��⹹��Ĳ����б���
;; (destructuring-bind ((a &optional (b 'bee)) one two three)
;;      `((alpha) ,@(iota 3)) 
;;    (list a b three two one)) => (ALPHA BEE 3 2 1)

;; 18.2 �����ṹ: �⹹�������κζ���⹹ 

;; 18.2.1 ͨ�����н⹹������
;; ��destruc�����ı���ƥ��ת����һϵ��Ƕ�׵�let�����ɴ��� 
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

;; ����ƥ��ģʽ����ÿ�������������ڶ�Ӧ�����λ�ù�����һ��
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
		  ;; һ���µı���(���ɷ���)�����󶨵�ÿ����������
		  (let ((var (gensym)))
		    (cons (cons `(,var (elt ,seq ,n))
				(destruc p var atom?))
			  rec))))))))

;; �ڶ��������������б������������ǵ��������
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

;; ��������ڸ�����������û�а��������ڴ���Ԫ�أ��⹹������������һ������
(dbind (a b c) (list 1 2)) ;; => SYSTEM::LIST-ELT: index 2 too large for (1 2)  

;; 18.2.2 ���� with-matrix�꣬���ڽ⹹��ά����
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
