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

;; 18.3 �ṹ��Ľ⹹

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; (mkstr pi " pieces of " 'pi) => "3.1415926535897932385L0 pieces of PI"

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
;; (symb 'ar "Madi" #\L #\L 0) => |ARMadiLL0| 

;; name: �ṹ��������Ϊǰ׺
;; field: �ֶ��� 
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

;; 18.3 ���õĽ⹹ 
(defclass thing ()
  ((x :initarg :x :accessor thing-x)
   (y :initarg :y :accessor thing-y)))
;; => #<STANDARD-CLASS THING> 
(setq thing (make-instance 'thing :x 0 :y 1))
;; => #<THING #x000335767B08>
;; with-slots����xʵ������thing��������x����ֱ�����ã�������
;; incf��ֱ���޸�thing�е�x��ֵ
(with-slots (x y) thing
  (incf x) (incf y)) 
(thing-x thing) ;; => 1
(thing-y thing) ;; => 2

;; ���ź� 
;; call-by-name/symbol/ref�� a��������thing��x���Եķ��ţ�����
(symbol-macrolet ((a (thing-x thing)))
  (setf a 100))
(thing-x thing) ;; => 100

;; call-by-value aֻ��һ���ֲ���������������
(let ((a (thing-x thing)))
  (setf a 200)) ;; => 200 
(thing-x thing) ;; => 100

;; with-slots���ǽ���symbol-macroletʵ�ֵ�,������let
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

;; dbind��һ����������һ��ֵ�ϣ���with-placesȴ�ǽ�����������һ�������ҵ�һ��ֵ��ָ�����, ÿһ�����ö���Ҫ����һ�β�ѯ,��ʵ��dbindҪ�죡������

;; 18.4 ƥ��: �⹹�Ǹ�ֵ�ķ�����ƥ���ǽ⹹�ķ���

;; (p ?x ?y c ?x)
;; (p a b c a)
;; �� ?x = a ���� ?y = b ʱƥ��

;; (p ?x b ?y a)
;; (p ?y b c a)
;; �� ?x = ?y = c ʱƥ��

;; Լ����ģʽ��������'��'��ʼ��, ����ͨ���޸�varsym?�������

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))
;; (varsym? 'y) => NIL
;; (varsym? '?y1) => T

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form))) 

;; x:ģʽ���������x������binds�У�����x��ֵ�Լ�����صİ�
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
   ;; x��yֵ��ͬ��_��ͨ��� ��һ������ֵ�ǵ�ǰ�İ󶨣��ڶ�������ֵ��T 
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t)) ;; rule 1
   ;; ģʽ����x�ڵ�ǰ�������ҵ���У��y�Ƿ��ǰ��е�ֵ��it��ֵ��(binding x binds)�ĵ�һ������ֵ�� ���ǵ�ǰ���б���x�󶨵�ֵ 
   ((binding x binds) (match it y binds)) ;; rule 2 -> rule1 
   ;; ģʽ����y�ڵ�ǰ�������ҵ���У��x�Ƿ��ǰ��е�ֵ
   ((binding y binds) (match x it binds)) ;; rule 3 -> rule1
   ;; ģʽ����x�޷��ڵ�ǰ���ҵ���Ӧ��ֵ����x��y������
   ((varsym? x) (values (cons (cons x y) binds) t)) ;; rule4 -> ִ��rule6��(match (cdr x) (cdr y)..
   ;; ģʽ����x�޷��ڵ�ǰ���ҵ���Ӧ��ֵ����x��y������
   ((varsym? y) (values (cons (cons y x) binds) t)) ;; rule5 -> ִ��rule6��(match (cdr x) (cdr y).. 
   ;; ���(car x) �� (car y)ƥ�䣬 ��ƥ�� (cdr x) (cdr y) 
   ((and (consp x) (consp y) (match (car x) (car y) binds)) 
    (match (cdr x) (cdr y) it)) ;; rule6
   ;; ��ƥ��
   (t (values nil nil)))) ;; rule7 


(match '(p a b c a) '(p ?x ?y c ?x))
;; => ((?Y . B) (?X . A)), T
;; rule6���� 
(and (consp '(p a b c a)) (consp '(p ?x ?y c ?x)) (match 'p 'p nil)) ;; => NIL,T
;; rule1����
(match 'p 'p nil) ;; => NIL,T 
(match '(a b c a) '(?x ?y c ?x)) 
;; rule5 ����
(match 'a '?x) ;;  => ((?X . A)), T  
(match '(b c a) '(?y c ?x)
       '((?X . A))) 
;; rule5 ����
(match 'b '?y '((?X . A))) ;; => ((?Y . B) (?X . A)), T 
(match '(c a) '(c ?x) '((?Y . B) (?X . A))) 
;; rule6����
(match 'c 'c '((?Y . B) (?X . A))) 
;; rule5 ����
(match 'a '?x '((?Y . B) (?X . A)))
;; rule4����
(binding '?x '((?Y . B) (?X . A))) ;; => A, (?X . A) 
(match 'a '?x '((?Y . B) (?X . A))) ;; => ((?Y . B) (?X . A)), T

;; ����ƥ�������
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test  
       (if (or it ,win) ,then ,else))))

;; pat: ģʽ��ģʽ����
;; seq: ���У������ֵ
;; then: ģʽƥ��ɹ����ִ�����
;; else: ģʽƥ��ʧ�ܺ��ִ�����
(defmacro if-match (pat seq then &optional else)
  ;; ͨ���Ƚ�ģʽ�������������� 
  `(aif2 (match ',pat ,seq)
	 ;; ��then����ǳ��ֵ�ģʽ�����б��滻�������ж�Ӧ�󶨵�ֵ
	 (let ,(mapcar #'(lambda (v)
			   `(,v (binding ',v it)))
		       (vars-in then #'atom))
	   ,then)
	 ,else))

;; ��ȡһ�����ʽ���ڳ��ֵ�ģʽ�����б�  
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

;; if-match �̣ܶ��������Ƿǳ���Ч. �������ڰ��������ж������ˣ����ܵ�һ�������ڱ����ھ�����֪�ġ��������ǣ��ڽ���ƥ��Ĺ����У��������б�����ű�����

