;; 25 面向对象

;; 不把 CLOS 仅仅看作一种编写面向对象程序的手段，而把它本身就当成一个 Lisp 程序。从这个角度来看待 CLOS 是理解 Lisp 和面向对象编程之间关系的关键

;; 25.1 万变不离其宗

;; 面向对象的编程意味着程序组织方式的一次变革，把传统的程序结构拆分开来：它不再让单一的程序逻辑去操纵那些被动的数据，而是让数据自己知道该做些什么，程序逻辑就隐含在这些新的数据 "对象" 间的交互过程之中

;; 编写单独的函数，让它检查参数的类型，然后分情况计算面积
;; (defun area (x)
;;   (cond ((rectangle-p x) (* (height x) (width x)))
;; 	((circle-p x) (* pi (expt (radius x) 2)))))

;; 面向对象，则是拆开这个函数，每条语句都被分到对象的对应类型中去。在这种模式下，可以向对象询问该对象的面积，然后对象则根据所属类型所提供的方法来作出回应

;; 比如rectangle类可能就会看起来像这样：
;;#'(lambda (x) (* (height x) (width x)))
;; 至于 circle 则会是这样：
;;#'(lambda (x) (* pi (expt (radius x) 2)))

;; 事实上Lisp本身包含的CLOS就是一个面向对象的扩展，从这个角度上说Lisp也是面向对象的。但是更准确地说：Lisp是一门可以用来面向语言的语言，CLOS只是这个特性的副产品！然而CLOS的庞大规模让我们很难认清这个事实，因此可以通过自己实现面向对象来更好地学习Lisp

;; 25.2 阳春版的Lisp中的对象
;; 面向对象编程，简而言之，就是：
;; 1. 具有属性的对象
;; 2. 能对各种消息作出反应
;; 3. 对象能从它的父对象继承相应的属性和方法

;; 在 Lisp 里面已经有好几种存放成组属性的方法。其中一种就是把对象实现成哈希表，把对象的属性作为哈希表里的表项。就可以用 gethash 来访问指定的属性：
;; (gethash 'color obj)

;; 由于函数是数据对象，同样可以把它们当作属性保存起来, 然后通过funcall或者apply来调用
;; (funcall (gethash 'move obj) obj 10)

;; 还有一种类似small talk的消息传递调用方式
(defun tell (obj message &rest args)
  (apply (gethash message obj) obj args))
;; 调用的形式就是：
;; (tell obj 'move 10)

;; 使用递归版本的gethash实现继承获取属性和方法
(defun rget-1 (obj prop)
  (multiple-value-bind (val win) (gethash prop obj)
    ;; 找到对应的属性或方法
    (if win
	(values val win) ;;获取对应的属性或方法
	(let ((par (gethash 'parent obj))) ;;获取父对象
	  (and par (rget par prop)))))) ;; 如果父对象不为空，则递归寻找对应的属性或方法

;; 指定对象的父类
;; (setf (gethash 'parent obj) obj2)

;; 但是只是有了单继承，即一个对象只能有一个父类。不过可以把parent属性改成一个列表，这样就能有多继承了

;;  d
;; | |
;; b c
;; | |
;;  a

;; a继承自b和c，而b和c均继承于d。深度优先（或叫高度优先）的遍历会依次走过a、b、|d|、c和d。倘若想要的属性同时存在于在 d和c里，那么我们将会得到d中的属性，而非c中的。这种情况会违反一个原则：即子类应当会覆盖基类中提供的缺省值。正确的搜索顺序应该是a、b、c、d

;; 多继承的rget采取的策略是构造一个列表，列表由原始对象的所有祖先构成，然后对列表排序，让列表中没有一个对象出现在它的子孙之前，最后再依次查看每个元素
;; (defun rget (obj prop)
;;   (some2 #'(lambda (a) (gethash prop a))
;; 	 (get-ancestors obj)))

;; (defun rget (obj prop)
;;   (some2 #'(lambda (a) (gethash prop a))
;;          (get-ancestors obj)))

;; (defun getall(x)
;;   (let ((parents (gethash 'parents x)))
;;     (cond ((null parents) nil)
;; 	  (t (append parents (mapcan #'getall parents))))))

;;收集所有的父类，然后排序
(defun get-ancestors (obj)
  (labels ((getall (x)
             (append (list x)
                     (mapcan #'getall
                             (gethash 'parents x)))))
    
    ;;为了避免在排序时把同一层次的祖先顺序打乱，get-ancestors 使用的是stable-sort而非sort
    (stable-sort (delete-duplicates (getall obj))
		 #'(lambda (x y)
		     (member y (gethash 'parents x))))))  

;;适用于 gethash 这类用第二个返回值表示成功或失败的函数
(defun some2 (fn lst)
  (if (atom lst)
      nil
      (multiple-value-bind (val win) (funcall fn (car lst))
	(if (or val win)
	    (values val win)
	    (some2 fn (cdr lst))))))

;; 为一个对象添加多个父类，注意：父类的优先级是从左到右降低的，也就是如果两个属性在y,z都有，那会使用y的
;; (setf (gethash 'parents x) (list y z))

;; (setq scoundrel (make-hash-table)
;;   patriot (make-hash-table)
;;   patriotic-scoundrel (make-hash-table))

;; (setf (gethash 'serves scoundrel) 'self
;;   (gethash 'serves patriot) 'country
;;   (gethash 'parents patriotic-scoundrel)
;;   (list scoundrel patriot))

;; (rget patriotic-scoundrel 'serves) ;; => SELF, T

;; 新建对象的函数
(defun obj-1 (&rest parents)
  (let ((obj (make-hash-table)))
    (setf (gethash 'parents obj) parents)
    (ancestors obj)
    obj))

(defun ancestors (obj)
  (or (gethash 'ancestors obj)
      (setf (gethash 'ancestors obj) (get-ancestors obj))))

;; 改进消息调用的语法

;; 每个属性定义成函数来去掉tell，可选参数meth?的值如果是真的话，那表示这个属性应该被当作方法来处理。否则它应该被当成一个slot，并径直返回rget 所取到的值
(defmacro defprop (name &optional meth?)
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
	    `(run-methods obj ',name args)
	    `(rget obj ',name)))
     (defsetf ,name (obj) (val)
       `(setf (gethash ',',name ,obj) ,val))))



;;　调用方法
(defun run-methods-1 (obj name args)
  (let ((meth (rget obj name)))
    (if meth 
	(apply meth obj args)
	(error "No ~A method for ~A." name obj))))

;; 添加方法find-owner　
;; (defprop find-owner t)
;; 为obj1添加方法find-owner 
;; (find-owner obj1) 

;; (macroexpand-1 '(defprop find-owner t))
;; (PROGN (DEFUN FIND-OWNER (OBJ &REST ARGS)
;; 	 (RUN-METHODS OBJ 'FIND-OWNER ARGS))
;;        (DEFSETF FIND-OWNER (OBJ) (VAL)
;; 	 (LIST 'SETF (LIST 'GETHASH ''FIND-OWNER OBJ) VAL)))

;; (progn
;;   (setq scoundrel (obj-1))
;;   (setq patriot (obj-1))
;;   (setq patriotic-scoundrel (obj scoundrel patriot))
;;   (defprop serves)
;;   (setf (serves scoundrel) 'self)
;;   (setf (serves patriot) 'country)
;;   (serves patriotic-scoundrel)) ;; => SELF, T

;; before方法会在该方法中其余部分运行前，作为前奏，被先行调用
;; after方法会作为收场在最后调用。
;; 在两者之间会执行曾经自己就是整个方法的函数，被称为主方法。它的返回值将被作为整个方法的返回值，即使after方法在其后调用
;; 如果存在around 方法，那么被调用的就不再是主方法，而是around方法。并且around方法有办法调用主方法，至于调不调则是它的自由

;; 加上辅助方法的支持，调用的顺序将变成这样：
;; 1. 倘若有的话，先是最匹配的around 方法
;; 2. 否则的话，依次是：
;; (a) 所有的before方法, 从最匹配的到最不匹配的
;; (b) 最匹配的主方法
;; (c) 所有的after方法, 从最不匹配的到最匹配的

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
;; 方法不再作为单个的函数出现，它成了有四个成员的结构
(defstruct meth around before primary after)

;;　field可以是'around', 'before', 'after', 'primary', 获取val这个结构体对应某种方法的代码
(defmacro meth- (field obj)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (and (meth-p ,gobj)
	    (,(symb 'meth- field) ,gobj)))))

;; (macroexpand-1 '(meth- around val)) 
;; => (LET ((#:G3852 VAL))
;;      (AND (METH-P #:G3852)
;; 	  (METH-AROUND #:G3852)))

;;寻找对应的属性或方法
(defun rget (obj prop &optional meth (skip 0))
  (some2 #'(lambda (a) 
	     (multiple-value-bind (val win) (gethash prop a)
	       (if win
		   (case meth (:around  (meth- around val))
			 (:primary (meth- primary val))
			 (t (values val win))))))
	 (nthcdr skip (ancestors obj))))

(defun run-methods (obj name args)
  ;; pri 是主方法
  (let ((pri (rget obj name :primary)))
    (if pri
	;; ar是around方法，如果存在around方法，则不调用主方法
	(let ((ar (rget obj name :around)))
	  (if ar
	      (apply ar obj args)
	      (run-core-methods obj name args pri)))
	(error "No primary ~A method for ~A." name obj))))

;; 依次调用before方法，主方法，和after方法
(defun run-core-methods-1 (obj name args &optional pri)
  (multiple-value-prog1
      (progn (run-befores obj name args)
	     (apply (or pri (rget obj name :primary)) 
		    obj args))
    (run-afters obj name args)))

(defun run-befores (obj prop args)
  ;; 寻找匹配的before方法，从最匹配到最不匹配的
  (dolist (a (ancestors obj))
    (let ((bm (meth- before (gethash prop a))))
      (if bm (apply bm obj args)))))

(defun run-afters (obj prop args)
  ;;寻找匹配的after方法，从最不匹配的到最匹配的
  (labels ((rec (lst)
	     (when lst
	       (rec (cdr lst))
	       (let ((am (meth- after 
				(gethash prop (car lst)))))
		 (if am (apply am (car lst) args))))))
    (rec (ancestors obj))))

;; 方法不再作为单个的函数出现，它成了有四个成员的结构。现在要定义一个主方法：
;; (setf (meth-primary (gethash 'move obj)) #'(lambda ...))

;; 定义方法宏
(defmacro defmeth ((name &optional (type :primary)) 
			   obj parms &body body)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (defprop ,name t)
       (unless (meth-p (gethash ',name ,gobj))
	 (setf (gethash ',name ,gobj) (make-meth)))
       (setf (,(symb 'meth- type) (gethash ',name ,gobj))
	     ,(build-meth name type gobj parms body)))))

;; around 和主方法可以使用call-next来调用下一个方法
(defun build-meth (name type gobj parms body)
  (let ((gargs (gensym)))
    `#'(lambda (&rest ,gargs)
	 (labels
	     ;; 由于call-next的行为取决于它被调用的地方，因此call-next不会用一个defun来在全局定义，只能在每个由 defmeth定义的方法里局部定义
	     ((call-next ()
		,(if (or (eq type :primary) 
			 (eq type :around))
		     `(cnm ,gobj ',name (cdr ,gargs) ,type)
		     '(error "Illegal call-next.")))
	      (next-p ()
		,(case type
		       (:around 
			`(or (rget ,gobj ',name :around 1)
			     (rget ,gobj ',name :primary))) 
		       (:primary 
			`(rget ,gobj ',name :primary 1))
		       (t nil))))
	   (apply #'(lambda ,parms ,@body) ,gargs)))))

;; 所谓下一个方法指的是倘若当前方法不存在，就会被调用的方法。
(defun cnm (obj name args type)
  (case type
    ;;around方法的下一个方法
    (:around  (let ((ar (rget obj name :around 1))) 
		(if ar ;; 次匹配的around方法
		    (apply ar obj args)
		    ;; 没有次匹配的around方法，那么就是由before,主方法，after构成的core方法
		    (run-core-methods obj name args))))
    ;; 主方法里下一个方法则会是第二匹配的主方法
    (:primary (let ((pri (rget obj name :primary 1)))
		(if pri
		    (apply pri obj args)
		    (error "No next method."))))))

;; (progn 
;;   (setq rectangle (obj)) 
;;   (defprop height)
;;   (defprop width)
;;   (defmeth (area) rectangle (r)
;;     (* (height r) (width r)))
;;   (let ((myrec (obj rectangle)))
;;   (setf (height myrec) 2
;;     (width myrec) 3)
;;   (area myrec)))

;; => 6

;; around 方法就会执行。它会运行平常时候在backup里运行的那些代码，不同之处是把它们放到了一个time的调用里执行。time的返回值则会被作为backup方法调用的值返回
;; (progn 
;;   (setq filesystem (obj))
;;   (defmeth (backup :before) filesystem (fs)
;;     (format t "Remember to mount the tape.~%"))
;;   (defmeth (backup) filesystem (fs)
;;     (format t "Oops, deleted all your files.~%")
;;     'done)
;;   (defmeth (backup :after) filesystem (fs)
;;     (format t "Well, that was easy.~%"))
;;   (defmeth (backup :around) filesystem (fs)
;;     (time (call-next)))
;;   (backup (obj filesystem)))

;; Remember to mount the tape.
;; Oops, deleted all your files.
;; Well, that was easy.
;; Real time: 2.3E-4 sec.
;; Run time: 0.0 sec.
;; Space: 18024 Bytes
;; => Done

;; 去掉方法
(defmacro undefmeth ((name &optional (type :primary)) obj)
  `(setf (,(symb 'meth- type) (gethash ',name ,obj)) 
         nil))

;;(undefmeth (backup :around) filesystem)

;; 维护父类和子类的联系：不再用gethash来获得父类和子类信息，而是分别改用操作符parents和children

;;children是个宏，因而它对于setf是透明的
(defmacro children (obj) 
  `(gethash 'children ,obj))

(defun parents (obj)
  (gethash 'parents obj))

;; 这个函数包揽了所有的相关工作，让新的双向链接系统能保持其一致性
(defun set-parents (obj pars)
  ;;把obj原来的parents中的obj引用删除
  (dolist (p (parents obj))
    (setf (children p) 
          (delete obj (children p))))
  ;; 设置pars为obj的父类
  (setf (gethash 'parents obj) pars)	  
  ;; 在pars加入子类obj
  (dolist (p pars)
    (pushnew obj (children p)))
  ;;更新所有obj的ancestors对象
  (maphier #'(lambda (obj) 
	       (setf (gethash 'ancestors obj) 
		     (get-ancestors obj)))
	   obj)
  pars)

;; parents函数的逆操作被defsetf定义为set-parents
(defsetf parents set-parents)

;; maphier函数的作用相当于继承树里的mapc，mapc对列表里每个元素运行一个函数，同样的maphier也会对对象和它所有的后代应用指定的函数，除非这些节点构成没有公共子节点的树，否则有的对象会被传入这个函数一次以上。在这里这不会导致问题因为调用多次get-ancestors和调用一次的效果是相同的
(defun maphier (fn obj)
  (funcall fn obj)
  (dolist (c (children obj))
    (maphier fn c)))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
    (setf (parents obj) parents)
    obj))

;; 方法的组合

;; 假设my-orange是orange的子类，而orange又是citrus的子类。如果props方法用在citrus上的返回值是（round acidic），相应的，orange的返回值是（orange sweet） ，my-orange 的结果是（dented）。可不可以让（props my-orange）能返回这些值的并集（dented orange sweet round acidic）？


;;defcomb定义方法的组合形式，它把方法名作为第一个参数，第二个参数描述了期望的组合方式。通常这第二个参数应该是一个函数。不过它也可以是 :progn :and :or 和 :standard 中的一个。如果使用前三个，系统就会用相应的操作符来组合主方法，用 :standard 的话，就表示我们想用以前的办法来执行方法。
(defmacro defcomb (name op)
  `(progn
     (defprop ,name t)
     (setf (get ',name 'mcombine)
           ,(case op
		  (:standard nil)
		  (:progn '#'(lambda (&rest args) 
			       (car (last args))))
		  (t op)))))

;; 如果被调用的方法没有名为mcombine的属性，那么一切如常。否则mcombine 应该是个函数比如+，或是个关键字比如 ：or
;; 前面一种情况，所有主方法返回值构成的列表会被送进这个函数
;; 如果是后者的情况，我们会用和这个关键字对应的函数对主方法一一进行操作。
(defun run-core-methods (obj name args &optional pri)
  (let ((comb (get name 'mcombine)))
    (if comb
        (if (symbolp comb)
            (funcall (case comb (:and #'comb-and)
			   (:or  #'comb-or))
                     obj name args (ancestors obj))
            (comb-normal comb obj name args))
        (multiple-value-prog1
	    (progn (run-befores obj name args)
		   (apply (or pri (rget obj name :primary))
			  obj args))
          (run-afters obj name args)))))

(defun comb-normal (comb obj name args)
  (apply comb
         (mapcan #'(lambda (a)
                     (let* ((pm (meth- primary 
                                       (gethash name a)))
                            (val (if pm 
                                     (apply pm obj args))))
		       (if val (list val))))
                 (ancestors obj))))

(defun comb-and (obj name args ancs &optional (last t))
  (if (null ancs)
      last
      (let ((pm (meth- primary (gethash name (car ancs)))))
        (if pm
            (let ((new (apply pm obj args)))
              (and new 
                   (comb-and obj name args (cdr ancs) new)))
            (comb-and obj name args (cdr ancs) last)))))

(defun comb-or (obj name args ancs)
  (and ancs
       (let ((pm (meth- primary (gethash name (car ancs)))))
         (or (and pm (apply pm obj args))
             (comb-or obj name args (cdr ancs))))))


;; (progn 
;;   (setq citrus (obj))
;;   (setq orange (obj citrus))
;;   (setq my-orange (obj orange))

;;   (defmeth (props) citrus (c) '(round acidic))
;;   (defmeth (props) orange (c) '(orange sweet))
;;   (defmeth (props) my-orange (m) '(dented))

;;   (defcomb props #'(lambda (&rest args) (reduce #'union args)))

;;   (props my-orange))
;; => (DENTED ORANGE SWEET ROUND ACIDIC)
