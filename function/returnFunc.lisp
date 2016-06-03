(defun make-adder (n)
  #'(lambda (x) (+ x n))) 

;; (setq add3 (make-adder 3)) 
;; (funcall add3 2) 
;; (apply add3 '(2)) 

(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))
