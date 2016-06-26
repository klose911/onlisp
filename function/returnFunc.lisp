(defun make-adder (n)
  #'(lambda (x) (+ x n))) 

;; (setq add3 (make-adder 3)) 
;; (funcall add3 2) 
;; (apply add3 '(2)) 

(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))


(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x) 
            (and (funcall fn x) (funcall chain x))))))

;; cdr�ݹ� 
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))
;; (our-length '(2 3 4)) ;; => 3 

(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
	   (our-every fn (cdr lst)))))

;; (our-every #'oddp '(1 3 5)) ;; => T

(defun our-rec (fn lst)
  (if (null lst)
      0
      (funcall fn (our-rec fn (cdr lst))))) 

;;(our-rec #'1+ '(2 3 4)) ;; => 3 

;; rec��һ����2�������ĺ���
;; ��һ�������ǣ��б�ĵ�ǰcar
;; �ڶ��������ǣ��ݹ�ĺ���
(defun lrec (rec &optional base)
  (labels ((self (lst)
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  #'(lambda ()
			      (self (cdr lst)))))))
    #'self))


(lrec #'(lambda(x f) (1+ (funcall f))) 0)
;; apply ��Ҫ�ٰ�װһ���б�
(apply (lrec #'(lambda(x f) (1+ (funcall f))) 0) '((2 3 4))) ;; 3 
(funcall (lrec #'(lambda(x f) (1+ (funcall f))) 0) '(2 3 4))  ;; 3 

;; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))
;; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))
;; find-if,for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))
;; some,for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))

;; 5.6 �������ϵݹ�: ���㿪ʼʹ��Ƕ���б�����ϣ���ݹ�ط����б��car��cdr֮ʱ
;; �б��ܱ�ʾ���У����ϣ�ӳ�䣬���飬 �Լ��������б���������������������������car������������cdr
;; (a b c) = (a . (b . (c . nil)))
;; (a b (c d)) = (a . (b . ((c . (d . nil)) . nil)))
(setq x '(a b)
      listx (list x 1))
;; copy-list���б���һ����������������������б��к������б���������ô���б���Ϊ�������Ԫ�أ��ǲ��ᱻ���Ƶ�
(eq x (car (copy-list listx))) ;; => T
;; copy-tree����б������������������б���Ϊ������ �������б�Ҳһ���ᱻ����
(eq x (car (copy-tree listx))) ;; => NIL

(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
	    (if (cdr tree) (our-copy-tree (cdr tree))))))

(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
	 (or (if (cdr tree) (count-leaves (cdr tree)))
	     1))))
;; (count-leaves '((a b (c d)) (e) f)) ;; ����4��NIL �ܹ���10��ԭ�� 
;; ((a b (c d)) (e) f) =  (. (a . (b . ((c . (d . nil)) . nil))) ((e . nil) . (f . nil)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))
(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
	     (if (cdr tree) (flatten (cdr tree))))))
;; (flatten '((a b (c d)) (e) f ())) ;; => (A B C D E F) 

(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
	  (if (cdr tree) (rfind-if fn (cdr tree))))))
;; (rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5)) ;; => 3

;; copy-tree ��count-leaves ��flatten��rfind-if����Щ���Ƶ�
;; ���tree��ԭ�ӣ��Դ����tree������ֵ����
;; ��֮�������������������Խ��еݹ���ã�Ȼ�����ߵĽ��������ֵ

;; ttrav(tree-traveser)
;; rec��һ�������� ��������Ĳ������ǵݹ���õķ���ֵ������ͨ�������Σ� ���ǻ������һ���������������������հ����հ��ֱ����б�ʾ���ò������������Ϳ��Ա�д��Щ���������Ƶݹ���̵ĵݹ麯����
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		     base)
		 (funcall rec (self (car tree))
			  (if (cdr tree)
			      (self (cdr tree)))))))
    #'self))

;; (funcall (ttrav #'cons)
;; 	 '((a b) c)) ;; => ((A B) C)
;; (funcall (ttrav #'(lambda (left right)
;; 		    (+ left (or right 1)))
;; 		1)
;; 	 '((a b (c d)) (e) f)) ;; => 10
;; (funcall (ttrav #'nconc #'mklist)
;; 	 '((a b (c d)) (e) f ())) ;; =>  (A B C D E F)

;; rfind-ifһ��������Ҫ�ҵ�Ԫ�ؾ�ֹͣ����
;; trec��rec����Ӧ����һ���������������ĺ�������ǰ�Ķ����Լ������ݹ����
;; ��������������������ʾ�������������������еݹ�������հ�
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	 (if (atom tree)
	     (if (functionp base)
		 (funcall base tree)
		 base)
	     (funcall rec tree
		      #'(lambda ()
			  (self (car tree))) ;; ��Ӧfuncall l 
		      #'(lambda ()
			  (if (cdr tree)
			      (self (cdr tree)))))))) ;; ��Ӧfuncall r
    #'self))

;; (funcall (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
;; 	       #'(lambda (tree) (and (oddp tree) tree))) ;; base
;; 	 '(2 (3 4) 5)) ;; 3 
