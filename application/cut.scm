(define *paths* ())

(define failsym '@) 

(define (choose choices)
  (if (null? choices)
      (fail)
      (call-with-current-continuation
       (lambda (cc)
	 (set! *paths*
	       (cons (lambda ()
		       (cc (choose (cdr choices))))
		     *paths*))
	 (car choices)))))

(define fail)

(call-with-current-continuation
 (lambda (cc)
   (set! fail
	 (lambda ()
	   (if (null? *paths*)
	       (cc failsym)
	       (let ((p1 (car *paths*)))
		 (set! *paths* (cdr *paths*))
		 (p1))))))) 

(define (find-boxes)
  (set! *paths* ())
  (let ((city (choose '(la ny bos))))
    (newline)
    (let* ((store (choose '(1 2)))
	   (box (choose '(1 2))))
      (let ((triple (list city store box)))
        (display triple)
        (if (coin? triple)
	    (display 'c))
        (fail)))))

(define (coin? x)
  (member x '((la 1 2) (ny 1 1) (bos 2 2))))

;; (find-boxes)
;; (la 1 1)(la 1 2)c(la 2 1)(la 2 2)
;; (ny 1 1)c(ny 1 2)(ny 2 1)(ny 2 2)
;; (bos 1 1)(bos 1 2)(bos 2 1)(bos 2 2)c
;; Value: @ 

;; 搜索树的剪枝作为一种优化技术却高度依赖choose的实现细节
;; 用函数fail本身做标记, 
(define (mark)
  (set! *paths* (cons fail *paths*)))

;; 调用cut让paths 一直退栈，直到弹出最新近压入的标记
(define (cut)
  (cond ((null? *paths*))
    ((eq? (car *paths*) fail)
      (set! *paths* (cdr *paths*)))
    (else
      (set! *paths* (cdr *paths*))
      (cut)))) 

;; 一旦找到了有硬币的盒子，我们就会从下一个城市继续我们的搜索
(define (find-boxes-mark-cut)
  (set! *paths* ())
  (let ((city (choose '(la ny bos))))
    (mark) ;; 以城市作为剪枝的标记
    (newline)
    (let* ((store (choose '(1 2)))
        (box (choose '(1 2))))
      (let ((triple (list city store box)))
        (display triple)
        (if (coin? triple)
          (begin (cut) (display 'c))) ; 找到后直接把*path*中的内容减少到上一个标记fail的地点
        (fail)))))

;; (find-boxes-mark-cut)
;; (la 1 1)(la 1 2)c
;; (ny 1 1)c
;; (bos 1 1)(bos 1 2)(bos 2 1)(bos 2 2)c
;; Value: @
