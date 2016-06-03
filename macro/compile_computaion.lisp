;; 13 编译器计算：如果把操作符实现成宏，就可以在展开时完成部分工作

;; 13.1 实用工具
(defun avg-fun (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg-macro (&rest args)
  `(/ (+ ,@args) ,(length args)))

;; (avg-fun pi 4 5) ;; => 4.0471975511965977462L0
;; (avg-macro pi 4 5) ;; => 4.0471975511965977462L0
;; (macroexpand-1 '(avg-macro pi 4 5)) ;; => (/ (+ PI 4 5) 3)  

;; 多数参数都为真的时候返回真 
(defun most-of-fun (&rest args)
  (let ((all 0)
	(hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro most-of-macro (&rest args)
  (let ((need (floor (/ (length args) 2)))
	(hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
			 `(and ,a (> (incf ,hits) ,need)))
		     args)))))

;; (most-of-fun 1 2 3) ;; => T
;; (most-of-macro 1 2 3) ;; => T
;; (most-of-fun nil nil 1) ;; => NIL
;; (most-of-macro nil nil 1) ;; => NIL

;; (macroexpand-1 '(most-of-macro a b c))
;; (floor (/ (length '(a b c)) 2)) ;; => 1 (need)    
;; => 
;; (LET ((#:G3409 0)) ;; (hits) 
;;   (OR (AND A (> (INCF #:G3409) 1))
;;       (AND B (> (INCF #:G3409) 1))
;;       (AND C (> (INCF #:G3409) 1)))) 

;; 由于sort是破坏性的，在排序之前先复制列表.
;; 它构造新的点对，而且对整个参数列表排序, 效率比较差 
(defun nthmost-fun (n lst)
  (nth n (sort (copy-list lst) #'>)))
;; (nthmost-fun 2 '(2 6 1 5 3 4)) ;; => 4 

;; 如果你想要找到一个盘子里第三大的那块饼干，
;; 你可以依次查看每一块饼干, 同时保持手里总是拿着已知最大的三块，
;; 当你检查完所有的饼干之后，你手里最小的那块饼干就是你要找的了
(defmacro nthmost-macro (n lst)
  (if (and (integerp n) (< n 20)) ;; 如果第一个参数字面上不是一个数，它就被展开成和我们上面看到的相同的代码
      (let ((glst (gensym))
	    (gi (gensym))
	    (syms (map0-n #'(lambda (x) (gensym)) n)))
	`(let ((,glst ,lst))
	   (unless (< (length ,glst) ,(1+ n))
	     ,@(gen-start glst syms)
	     (dolist (,gi ,glst)
	       ,(nthmost-gen gi syms t))
	     ,(car (last syms)))))
       `(nth ,n (sort (copy-list ,lst) #'>))))

;; 生成先拿出n块饼干的代码
(defun gen-start (glst syms)
  (reverse
   (maplist #'(lambda (syms)
		(let ((var (gensym)))
		  `(let ((,var (pop ,glst)))
		     ,(nthmost-gen var (reverse syms)))))
	    (reverse syms))))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

;; 生成遍历每一块饼干的代码
(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
	(if (and (not long?) (null else))
	    `(setq ,(car vars) ,var)
	    `(if (> ,var ,(car vars))
		 (setq ,@(mapcan #'list
				 (reverse vars)
				 (cdr (reverse vars)))
		       ,(car vars) ,var)
		 ,else)))))

;; (nthmost-macro 2 '(2 6 1 5 3 4)) ;; => 4 

;; (macroexpand-1 '(nthmost-macro 2 '(2 6 1 5 3 4))) 
;; (LET ((#:G3415 '(2 6 1 5 3 4)))
;;  (UNLESS (< (LENGTH #:G3415) 3)
;;   (LET ((#:G3422 (POP #:G3415))) (SETQ #:G3417 #:G3422))
;;   (LET ((#:G3421 (POP #:G3415)))
;;    (IF (> #:G3421 #:G3417) (SETQ #:G3418 #:G3417 #:G3417 #:G3421)
;;     (SETQ #:G3418 #:G3421)))
;;   (LET ((#:G3420 (POP #:G3415)))
;;    (IF (> #:G3420 #:G3417)
;;     (SETQ #:G3419 #:G3418 #:G3418 #:G3417 #:G3417 #:G3420)
;;     (IF (> #:G3420 #:G3418) (SETQ #:G3419 #:G3418 #:G3418 #:G3420)
;;      (SETQ #:G3419 #:G3420))))
;;   (DOLIST (#:G3416 #:G3415)
;;    (IF (> #:G3416 #:G3417)
;;     (SETQ #:G3419 #:G3418 #:G3418 #:G3417 #:G3417 #:G3416)
;;     (IF (> #:G3416 #:G3418) (SETQ #:G3419 #:G3418 #:G3418 #:G3416)
;;      (IF (> #:G3416 #:G3419) (SETQ #:G3419 #:G3416) NIL))))
;;   #:G3419))

;; 当某些参数在编译期已知时，你可以用宏来生成更高效的代码, 是否利用这种可能性取决于你想获得多少好处，以及你另外愿意付出多少努力来编写一个高效的宏版本!!!

;; 13.2 
