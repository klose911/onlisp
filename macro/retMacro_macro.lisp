;; 16 �����ĺ�
;; 16.1 ������

;; ʹ��rest ��,@�Ĺ��÷������Ѿ���Ϊ����һ���������꣬����special form��������������
(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))
(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

;; Ϊ�˶���һ�������ĺ꣬Ҫ�õ�Ƕ�׵ķ�����

;; �� multiple-value-bind�ӷ��������������Ļ�
;; (defmacro mvbind (&rest args)
;;   (let ((name 'multiple-value-bind))
;;     `(,name ,@args)))

;; �ѿɱ�ı��ʽ�滻�ɱ���, mvbind->,short, multiple-value-bind -> ,long
;; `(defmacro ,short (&rest args)
;;   (let ((name ',long))
;;     `(,name ,@args)))

;; �Ѵ���name��short��long���ڲ㷴�������Ƶ����abrev�Ĳ����б��У����򻯱��ʽ
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,`,long ,@args)))

;; (abbrev dbind destructuring-bind) �ȼ�����rest��,@������

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(abbrev ,@pair))
	       (group names 2))))

;; (abbrevs dbind destructuring-bind
;;   mvbind multiple-value-bind
;;   mvsetq multiple-value-setq) 

;; 16.2 ����
