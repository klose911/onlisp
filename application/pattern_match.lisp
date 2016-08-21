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

;; �ڶ��������������б������������ǵ��������
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
	 (cond ((gensym? pat) ;;���ģʽ������һ�����ɷ��ţ���ô����һ���� destruc �������ڱ������б�Ĳ��ɼ���������������������Ҫ�����������ľ��ǲ������Ƿ������ȷ�ĳ���
		`(let ((,pat ,expr))
		   (if (and (typep ,pat 'sequence)
			    ,(length-test pat rest))
		       ,then
		       ,else)))
	       ((eq pat '_) then) ;; ͨ�������Ҫ���κε�����
	       ((var? pat) ;; 
		(let ((ge (gensym)))
		  `(let ((,ge ,expr))
		     ;; ģʽ������δ��ֵ����ģʽ�����Ѿ��ͱ��ʽ�ж�Ӧ��ֵ��ͬ
		     (if (or (gensym? ,pat) (equal ,pat ,ge))
			 ;; ģʽ������Ϊ���ʽ��ֵ
			 (let ((,pat ,ge)) ,then)
			 ,else))))
	       (t `(if (equal ,pat ,expr) ;; һ�������ϵ�ֵ�����ɴ���ȥ�Ƚ����������еĶ�Ӧ����
		       ,then ,else)))))


;; pat: 'A, expr: (ELT G 1)  , rest: NIL
;; pat�ǳ��������ù���4 
;;(match1 '(('A (ELT G 1))) '(print ?x) nil) 
;; => (IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL)

;; pat: ?X expr: (ELT G 0), rest: NIL
;; pat��ģʽ���������ù���3�� ?X �󶨵���Ӧ�ı��ʽ(ELT G 0) 
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

;; (gen-match '((G SEQ) ((?X (ELT G 0)) ('A (ELT G 1)))) '(print ?X)) ;; ���õ�a  
;; һ·�ݹ鵽�б�Ľ�β��Ȼ���ڻ�����·�Ϲ����䷵��ֵ

;; (let ((then (gen-match '(((?X (ELT G 0)) ('A (ELT G 1)))) '(print ?X))) ;; ���õ�b 
;;     ....

;; (let ((then (gen-match '(('A (ELT G 1)))) '(print ?X))) ;; ���õ�c
;;     ....

;; (let ((then (gen-match nil '(print ?X))))
;; gen-match��������Ԫ�أ�then����(print ?x) , ����ִ�е��õ�c

;; (simple? (caar '(('A (ELT G 1)))) => T 
;; (match1 '(('A (ELT G 1))) '(print ?x) nil)
;; => (IF (EQUAL 'A (ELT G 1)) (PRINT ?X) NIL) ��Ϊ����ֵthen���������õ�b

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
;; 	    (PRINT ?X) NIL)) NIL)) ��Ϊ����ֵ���������õ�a

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
;; g��һ�����ɵ�symbol�����ù���1, ���г���У��

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
  ;; Ϊÿ��ģʽ��������symbol 
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

;; ������if-matchչ��ʽ 
;; (let ((?x '#:g1)) ;; ������?x�ڿ�ʼ�ı��󶨵���һ�� gensym���Ա�������δ��ƥ���������һ��ֵ��
;;   (labels ((#:g3 nil nil)) 
;;      match1�Ĺ���1��У��ģʽ�����Ƿ�ͱ��ʽ����һ��
;;     (let ((#:g2 seq)) ;;��Щ��������������gensym���ɵģ�Ŀ����Ϊ�˱��Ⲷ׽
;;       (if (and (typep #:g2 'sequence)
;;           (= (length #:g2) 2))
;;         match1�Ĺ���3����Xδ��ֵ��ģʽ������ֵΪ���ʽ�ĵ�һ��Ԫ��
;;         (let ((#:g5 (elt #:g2 0))) 
;;           (if (or (gensym? ?x) (equal ?x #:g5))
;;             (let ((?x #:g5))
;;              match1�Ĺ���4�� У��ģʽ�ĵڶ���Ԫ���Ƿ�����ʽ�ĵڶ���Ԫ����ͬ
;;               (if (equal 'a (elt #:g2 1))
;;                 (print ?x)
;;                 (#:g3)))
;;             (#:g3)))
;;         (#:g3)))))

;; ���µ� if-match �У�ģʽԪ�������Ǳ���ֵ�������Ǳ���ʽ�����ˡ�����ζ��Lisp�������Ա�����ģʽ�У��ͱ����õı��ʽһ����
(let ((n 3))
  (if-match-fast (?x n 'n '(a b)) '(1 3 n (a b))
		 ?x)) ;; => 1

;; ����������һ���ĸĽ�������Ϊ�°汾������ destruc �����֡�����ģʽ�п��԰��� &rest ���� &body
;; ������Ϊ destruc ʹ����һ������в����� elt �� subseq, ���������κ����͵�������

(defun abab (seq)
  (if-match-fast (?x ?y ?x ?y) seq
    (values ?x ?y)
    nil))

;; �ַ���
(abab "abab") ;; => #\a, #\b 
;; ���� 
(abab #(1 2 1 2)) ;; => 1, 2 

(if-match-fast (?x (1 . ?y) . ?x) '((a b) #(1 2 3) a b)
  (values ?x ?y)) ;; => (A B), #(2 3) 
