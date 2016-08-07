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

;; �������ļ�֦��Ϊһ���Ż�����ȴ�߶�����choose��ʵ��ϸ��
;; �ú���fail���������, 
(define (mark)
  (set! *paths* (cons fail *paths*)))

;; ����cut��paths һֱ��ջ��ֱ���������½�ѹ��ı��
(define (cut)
  (cond ((null? *paths*))
    ((eq? (car *paths*) fail)
      (set! *paths* (cdr *paths*)))
    (else
      (set! *paths* (cdr *paths*))
      (cut)))) 

;; һ���ҵ�����Ӳ�ҵĺ��ӣ����Ǿͻ����һ�����м������ǵ�����
(define (find-boxes-mark-cut)
  (set! *paths* ())
  (let ((city (choose '(la ny bos))))
    (mark) ;; �Գ�����Ϊ��֦�ı��
    (newline)
    (let* ((store (choose '(1 2)))
        (box (choose '(1 2))))
      (let ((triple (list city store box)))
        (display triple)
        (if (coin? triple)
          (begin (cut) (display 'c))) ; �ҵ���ֱ�Ӱ�*path*�е����ݼ��ٵ���һ�����fail�ĵص�
        (fail)))))

;; (find-boxes-mark-cut)
;; (la 1 1)(la 1 2)c
;; (ny 1 1)c
;; (bos 1 1)(bos 1 2)(bos 2 1)(bos 2 2)c
;; Value: @
