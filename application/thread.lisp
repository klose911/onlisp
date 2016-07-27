;; 21 多线程

;; 21.1 进程抽象

;; 函数 由前一章的 =defun 或者 =lambda 宏定义

;; 进程 由函数调用实例化
;; 活动进程的数量和一个函数能够实例化的进程数量都没有限制。每个进程有一个优先级，初始值由创建时给出的参数指定

;; 等待表达式(Waitexpressions) 等待表达式接受一个变量，一个测试表达式和一段代码体
;; 如果进程遇到等待表达式，进程将在这一点被挂起，直到测试表达式返回真。一旦进程重新开始执行，代码体会被求值，变量则被绑定到测试表达式的值
;; 测试表达式通常不应该有副作用，因为它被求值的时间和频率没有任何保证。

;; 调度 通过优先级来完成
;; 在所有能够重新开始执行的进程中，系统会运行优先级最高的进程。

;; 默认进程 在其他进程都不能执行时运行
;; 它是一个 read-eval-print 循环。

;; 创建和删除 绝大多数对象的操作可以即时进行
;; 正在运行中的进程可以定义新的函数，实例化或者杀死进程。

;; 21.2

;; pri : 进程的优先级，它应该是一个正数
;; state: 是一个续延，它用来表示一个挂起进程的状态。可以funcall一个进程的 state 来重新启动它
;; wait: 通常是一个函数，如果要让进程重新执行，它必须返回真，但刚创建的进程的wait为 nil, 为空的进程总是可以被重新执行
(defstruct proc pri state wait)

;; 全局变量 *procs*: 当前所有的进程列表
;; 全局变量 *proc*:  当前正在执行的进行
(proclaim `(special *procs* *proc*))

;; (defun declare-variable-types-globally (type vars)
;;   (proclaim `(type ,type ,@vars))
;;   type)
;; Once this form is executed, the dynamic variable *TOLERANCE*
;; must always contain a float.
;;(declare-variable-types-globally 'float '(*tolerance*)) ;; => FLOAT

(defvar *halt* (gensym))

;; 全局变量*default-proc*: 默认进程 
;; pri和wait都是nil 
(defvar *default-proc*
  (make-proc :state #'(lambda (x)
			(format t "~%>> ")
			;; 默认进程显式地调用了eval， 在其他情况下应该尽量避免。效率低下，功能有限 
			(princ (eval (read)))
			(pick-process))))

;;使用一个函数调用来实例化进程
(defmacro my-fork (expr pri)
  `(prog1 ',expr
     ;; 一个新进程被加入到了*procs*里面
     (push (make-proc
	    :state #'(lambda (,(gensym))
		       ,expr
		       (pick-process))
	    :pri ,pri)
	   *procs*)))

;; (=defun foo (x)
;;   (format t "Foo was called with ~A.~%" x)
;;   (=values (1+ x)))
;; (foo 100) ;; => 101 
;; (macroexpand-1 '(fork (foo 100) 25))
;; => (PROG1 '(FOO 100)
;;   (PUSH (MAKE-PROC :STATE #'(LAMBDA (#:G3517) (FOO 100) (PICK-PROCESS))
;; 		   :PRI 25)
;; 	*PROCS*))

;;定义成program的一组进程不返回任何值，只应该在 toplevel 被调用
(defmacro program (name args &body body)
  `(=defun ,name ,args
     (setq *procs* nil)
     ,@body
     ;; 把控制流抛(throw) 到*halt*的话，就会终止这个程序(program)。因为这个*halt*是个生成符号，不会与用户设置的 tag 冲突
     (catch *halt* (loop (pick-process)))))

;; (macroexpand-1 '(program two-foos (a b)
;; 		 (fork (foo a) 99)
;; 		 (fork (foo b) 99)))
;; (=DEFUN TWO-FOOS (A B)
;;   (SETQ *PROCS* NIL)
;;   (FORK (FOO A) 99)
;;   (FORK (FOO B) 99)
;;   (CATCH *HALT* (LOOP (PICK-PROCESS)))) 

;; 程序调度
;; 在可以继续执行的进程中，选出优先级最高的一个，然后运行它
(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
      *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

;; 如果一个挂起的进程没有wait函数或者它的 wait 函数返回真，那么它就被允许运行
;; 在所有被允许运行的进程中，具有最高优先级的被选中。胜出的进程和它的 wait 函数(如果有的话) 返回的值被返回给 pick-process
;; 获胜进程总是存在，因为默认进程总是可以被执行
(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
        (if (> pri max)
          (let ((val (or (not (proc-wait p))
                  (funcall (proc-wait p)))))
            (when val
              (setq proc1 p
                max pri
                val1 val))))))
    (values proc1 val1)))

;; 把某个进程放进进程表
(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
    (proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))


(defmacro my-wait (parm test &body body)
  `(arbitrator #'(lambda () ,test) ;; 测试函数
    #'(lambda (,parm) ,@body))) ;; 执行函数

(defvar *open-doors* nil)

;; 等待*open-doors*有值， 则输出Entering (car *open-doors*) 
(=defun pedestrian () 
  (my-wait d (car *open-doors*)
    (format t "Entering ~A~%" d)))
;; (macroexpand-1 ' (my-wait d (car *open-doors*)
;;     (format t "Entering ~A~%" d))) 
;; (ARBITRATOR 
;;  #'(LAMBDA NIL (CAR *OPEN-DOORS*))
;;  #'(LAMBDA (D) (FORMAT T "Entering ~A~%" D))) 

;; 把pedestrian加入进程表 
(program ped ()
  (my-fork (pedestrian) 1))

;; (macroexpand-1 '(program ped ()
;;   (my-fork (pedestrian) 1))) 
;;  (=DEFUN PED NIL 
;;    (SETQ *PROCS* NIL) 
;;    (MY-FORK (PEDESTRIAN) 1) 
;;    (CATCH *HALT* (LOOP (PICK-PROCESS))))  

;; (macroexpand-1 ' (my-fork (pedestrian) 1)) 
;; (PROG1 '(PEDESTRIAN) 
;;   (PUSH (MAKE-PROC 
;; 	 :STATE #'(LAMBDA (#:G3472) 
;; 		    (PEDESTRIAN) 
;; 		    (PICK-PROCESS)) 
;; 	 :PRI 1) 
;; 	*PROCS*)) 

;; (ped) ;; 启动
;; (push 'door1 *open-doors*) 
;; (push 'door2 *open-doors*) 
;; (my-halt) ;; 停止

;; 让其他更高优先级的进程有机会运行 
(defmacro my-yield (&body body)
  `(arbitrator nil ;; wait为空，永远可以执行
	       #'(lambda (,(gensym)) ,@body))) ;; 执行函数

(defun setpri (n) (setf (proc-pri *proc*) n))

;; 抛出*halt*
(defun my-halt (&optional val)
  (throw *halt* val))

(defun my-kill-proc (&optional obj &rest args)
  (if obj
    (setq *procs* (apply #'delete obj *procs* args))
    (pick-process)))


;; wait sample 
(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
  (format t "Approach ~A. " door)
  (claim 'knock door)
  (my-wait d (check 'open door)
    (format t "Enter ~A. " door)
    (unclaim 'knock door)
    (claim 'inside door)))

(=defun host (door)
  (my-wait k (check 'knock door)
    (format t "Open ~A. " door)
    (claim 'open door)
    (my-wait g (check 'inside door)
      (format t "Close ~A.~%" door)
      (unclaim 'open door))))

(program ballet ()
  (my-fork (visitor 'door1) 1)
  (my-fork (host 'door1) 1)
  (my-fork (visitor 'door2) 1)
  (my-fork (host 'door2) 1))

;; (ballet) 
;; (my-halt) 
