;; 16 定义宏的宏
;; 16.1 缩略语

;; 使用rest 和,@的惯用法，就已经能为任意一个函数、宏，或者special form定义其缩略语了
(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))
(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

;; 为了定义一个定义宏的宏，要用到嵌套的反引用

;; 把 multiple-value-bind从反引用里拉出来的话
;; (defmacro mvbind (&rest args)
;;   (let ((name 'multiple-value-bind))
;;     `(,name ,@args)))

;; 把可变的表达式替换成变量, mvbind->,short, multiple-value-bind -> ,long
;; `(defmacro ,short (&rest args)
;;   (let ((name ',long))
;;     `(,name ,@args)))

;; 把代表name的short，long从内层反引用中移到外层abrev的参数列表中，来简化表达式
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,`,long ,@args)))

;; (abbrev dbind destructuring-bind) 等价与用rest和,@来定义

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

;; 16.2 属性
