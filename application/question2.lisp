;; [示例代码 6.5]
;; 编译成闭包形式的网络
(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name  *nodes*)
	(if yes
	    #'(lambda ()
		(format t "~A~%>> " conts)
		(case (read)
		  (yes (funcall (gethash yes *nodes*)))
		  (t (funcall (gethash no *nodes*)))))
	    #'(lambda () conts)))) 

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln) 

;;(funcall (gethash 'people *nodes*))
