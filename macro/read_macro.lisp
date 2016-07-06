;; 17 ��ȡ��
;; ��Lisp���ʽ��һ���У�����������Ҫ��ʱ�̣��ֱ��Ƕ�ȡ�ڣ������ں�������
;; ������: ��������
;; ������: ��ת������
;; ��ȡ��: ��ȡ�������� 

;; 17.1 ���ַ�
;; reader����Ϊ������Щ����ʱ�ı�����Ժͱ������Ƶ�, ��Ҫ�ı�����Ϊ����򵥵ķ�ʽ���Ƕ����µĺ��ַ�
;; ���ַ���һ�ֱ� Lisp reader����Դ����ַ�
;; �ٸ����ӣ�Сд��ĸ a �Ĵ���ʽ��Сд��ĸ b ��һ���ģ����Ƕ��ɳ���Ĵ���ʽ����
;; �������ž���Щ��ͬ��������Lisp��ʼ��ȡһ���б�!!!!!! 
;; ÿ���������ַ�����һ����֮�����ĺ���������Lisp reader���������ַ���ʱ����ʲô
;; ���Ըı�һ�����еĺ��ַ��Ĺ������������߶������Լ����µĺ��ַ�
;; ���ú���set-macro-character�ṩ��һ�ֶ����ȡ��ķ�ʽ��������һ���ַ���һ���������Ժ�read��������ַ�ʱ�����ͷ��ص��øú����Ľ��

;; �����ŵĶ�ȡ�� 
(set-macro-character #\'
		     #'(lambda (stream char)
			  ;; �ú������������ĵڶ����βΣ���Ϊ�������Ǹ������ַ�'!!!!
			 (declare (ignore char))
			 ;; read����������������Ƿ�������end-of-fileʱ����
			 ;;                  ���������Ļ�����ʲôֵ��
			 ;;                  read�����Ƿ��Ƿ����ڵݹ�read������
			 (list 'quote (read stream t nil t))))
;; (defmacro q (s)
;;   (list 'quote s))
;; (q a) 
;; (defmacro q-1 (s)
;;   `(quote ,s))
;; (q-1 a)

;; ��ȡ��ͳ����һ������ʵ�ʶ��Ǻ���
;; �����ɺ�չ���ĺ���һ�����ͺ��ַ���صĺ�������������������ȡ�������⣬��Ӧ����������������!!!!

;; ��Ͷ�ȡ���ڲ�ͬ�Ľ׶η����͹۲���ĳ���
;; ���ڳ����з�������ʱ�����Ѿ���reader�������� Lisp ����
;; ����ȡ���ڳ������ı��Ľ׶�ʱ���Ͷ���ʩ��Ӱ����!!!!!!

;; ��ȡ��������������ȳ�����Ϊǿ�󡣶�ȡ�����Ӱ��Lisp��ȡ��ÿһ������������ֻ���ڴ����ﱻչ��
;; ��ȡ��ͨ���ݹ�ص���read
;; ''a �ᱻ��ȡ���� (quote (quote a))
;; (q (q a)) ����� (Q A) !!!!! �������������ȷ�����Ƕ���һ����������

;; 17.2 dispatching���ַ�
;; #'������#��ͷ�Ķ�ȡ��һ������һ�ֳ�Ϊdispatching��ȡ���ʵ������Щ��ȡ���������ַ����֣����е�һ���ַ���Ϊdispatch �ַ�
;; ������Ŀ�ģ���˵���Ǿ����ܵس������acii�ַ��������ֻ�е��ַ���ȡ��Ļ�����ô��ȡ��������ͻ��������ַ����Ĵ�С

;; #?�����ڶ��峣�������Ķ�ȡ�� 
(set-dispatch-macro-character #\# #\?
  #'(lambda (stream char1 char2)
    (declare (ignore char1 char2))
    `#'(lambda (&rest ,(gensym))
	 ,(read stream t nil t))))

;; #?2 ������ɷ��س���2�ĺ���
;; (mapcar #?2 '(a b c)) => (2 2 2)
;; ��#�����ַ��Ķ�����ʹ�ú��ַ�����ȫû������ģ����ʽ����ȡ����Щ���ַ�����ʧ��
;; (eq (funcall #?'a) 'a) => T 
;; (eq (funcall #?#'oddp) (symbol-function 'oddp)) => T

;; 17.3 �����
;; ��������#[x y]�ı��ʽ��ʹ�������ı��ʽ����ȡΪ��x��y�ı������������������б�
(set-macro-character #\] (get-macro-character #\)))
  (set-dispatch-macro-character #\# #\[
   #'(lambda (stream char1 char2)
     (declare (ignore char1 char2))
     (let ((accum nil)
	   ;; read-delimited-list�����ĵ�һ���������Ǹ��������б��β���ַ�]
        (pair (read-delimited-list #\] stream t)))
      (do ((i (ceiling (car pair)) (1+ i)))
        ((> i (floor (cadr pair)))
           (list 'quote (nreverse accum)))
	(push i accum)))))
;; #[2 7] => (2 3 4 5 6 7)

;; �� defdelim ���������ַ���һ�������б��Լ�һ����������
;; �����б�ʹ���������ʽ�ض�����һ������
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
    ;; ��ȡ���ڶ����ַ�Ϊֹ
    (set-macro-character right rpar)
    ;; �׸��ַ�����Ϊdispatching��ȡ��
    (set-dispatch-macro-character #\# left
				  #'(lambda (stream char1 char2)
				      (declare (ignore char1 char2))
				      ;; ������fnӦ�õ�����������
				      (apply fn
					     (read-delimited-list right stream t))))))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

;; �����������涨��߽��[]�ĺ�ȼ�
(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))
;; #[1 5] => (1 2 3 4 5)

;; ������ list �� 1+ ���������ú���ʱ��û�����ɵȵ������ڲ�ȥ��compose�ĵ�����ֵ
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns 
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
	  (build-compose (cdr expr))
	  (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
			  `(,(rbuild f) ,g))
		      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
		      (if fns
			  `(,(rbuild (car fns))
			     ,(rec (cdr fns)))
			  g)))
		(rec fns)))))

(defmacro fn (expr)
  `#',(rbuild expr))

;; ���������ɸ��Ϻ����������� 
(defdelim  #\{ #\}  (&rest args)
  `(fn  (compose  ,@args)))
;; #{list 1+} ;;  #<FUNCTION :LAMBDA (#:G3497) (LIST (1+ #:G3497))>
;; (funcall #{list 1+} 7) ;; => (8)

;; 17.4 ��Щ�����ں�ʱ
;; ��ȡ�����ڳ����֮ǰ���õĻ�����ô��������չ���ɺ��ж�ȡ��ı��ʽ����?
(defmacro quotable ()
  `(list 'able))
;; �����ǣ�����궨���е��������������defmacro���ʽ����ȡʱ���Ͷ���չ���ˣ�չ���������
(defmacro quotable ()
  (quote (list (quote able))))
;; �ں�չ��ʽ�������ȡ����û��ʲô����ġ���Ϊһ����ȡ��Ķ����ڶ�ȡ�ںͱ�����֮�佫���ᣨ����˵��Ӧ�ã������仯!!!
