(defun make-adder (n)
  #'(lambda (x) (+ x n))) 

;; (setq add3 (make-adder 3)) 
;; (funcall add3 2) 
;; (apply add3 '(2)) 

(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))


;; cdr递归 
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

;; rec：一个带2个参数的函数
;; 第一个参数是：列表的当前car
;; 第二个参数是：递归的函数
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
;; apply 需要再包装一层列表
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




