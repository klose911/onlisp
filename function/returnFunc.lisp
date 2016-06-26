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

;; 5.6 在子树上递归: 当你开始使用嵌套列表，而且希望递归地访问列表的car和cdr之时
;; 列表能表示序列，集合，映射，数组， 以及树。把列表看作二叉树，二叉树的左子树是car，右子树则是cdr
;; (a b c) = (a . (b . (c . nil)))
;; (a b (c d)) = (a . (b . ((c . (d . nil)) . nil)))
(setq x '(a b)
      listx (list x 1))
;; copy-list把列表当作一个序列来处理，即如果碰到列表中含有子列表的情况，那么子列表作为序列里的元素，是不会被复制的
(eq x (car (copy-list listx))) ;; => T
;; copy-tree会把列表当成树来拷贝，把子列表视为子树， 所以子列表也一样会被复制
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
;; (count-leaves '((a b (c d)) (e) f)) ;; 加上4个NIL 总共有10个原子 
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

;; copy-tree ，count-leaves ，flatten和rfind-if有这些相似点
;; 如果tree是原子，对传入的tree进行求值操作
;; 反之对左子树和右子树各自进行递归调用，然后两者的结果进行求值

;; ttrav(tree-traveser)
;; rec是一个函数， 这个函数的参数将是递归调用的返回值。对于通常的情形， 我们会改用另一个函数，让它接受两个闭包，闭包分别自行表示调用操作。这样，就可以编写那些能自主控制递归过程的递归函数了
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

;; rfind-if一发现它所要找的元素就停止遍历
;; trec的rec参数应当是一个具有三个参数的函数：当前的对象，以及两个递归调用
;; 后两个参数将是用来表示对左子树和右子树进行递归的两个闭包
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	 (if (atom tree)
	     (if (functionp base)
		 (funcall base tree)
		 base)
	     (funcall rec tree
		      #'(lambda ()
			  (self (car tree))) ;; 对应funcall l 
		      #'(lambda ()
			  (if (cdr tree)
			      (self (cdr tree)))))))) ;; 对应funcall r
    #'self))

;; (funcall (trec #'(lambda (o l r) (or (funcall l) (funcall r)))
;; 	       #'(lambda (tree) (and (oddp tree) tree))) ;; base
;; 	 '(2 (3 4) 5)) ;; 3 
