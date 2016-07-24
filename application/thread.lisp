;; 21 ���߳�
(load "continuation.lisp")

;; 21.1 ���̳���

;; ���� ��ǰһ�µ� =defun ���� =lambda �궨��

;; ���� �ɺ�������ʵ����
;; ����̵�������һ�������ܹ�ʵ�����Ľ���������û�����ơ�ÿ��������һ�����ȼ�����ʼֵ�ɴ���ʱ�����Ĳ���ָ��

;; �ȴ����ʽ(Waitexpressions) �ȴ����ʽ����һ��������һ�����Ա��ʽ��һ�δ�����
;; ������������ȴ����ʽ�����̽�����һ�㱻����ֱ�����Ա��ʽ�����档һ���������¿�ʼִ�У�������ᱻ��ֵ�������򱻰󶨵����Ա��ʽ��ֵ
;; ���Ա��ʽͨ����Ӧ���и����ã���Ϊ������ֵ��ʱ���Ƶ��û���κα�֤��

;; ���� ͨ�����ȼ������
;; �������ܹ����¿�ʼִ�еĽ����У�ϵͳ���������ȼ���ߵĽ��̡�

;; Ĭ�Ͻ��� ���������̶�����ִ��ʱ����
;; ����һ�� read-eval-print ѭ����

;; ������ɾ�� �����������Ĳ������Լ�ʱ����
;; ���������еĽ��̿��Զ����µĺ�����ʵ��������ɱ�����̡�

;; 21.2

;; pri : ���̵����ȼ�����Ӧ����һ������
;; state: ��һ�����ӣ���������ʾһ��������̵�״̬������funcallһ�����̵� state ������������
;; wait: ͨ����һ�����������Ҫ�ý�������ִ�У������뷵���棬���մ����Ľ��̵�waitΪ nil, Ϊ�յĽ������ǿ��Ա�����ִ��
(defstruct proc pri state wait)

;; ȫ�ֱ��� *procs*: ��ǰ���еĽ����б�
;; ȫ�ֱ��� *proc*:  ��ǰ����ִ�еĽ���
(proclaim `(special *procs* *proc*))

;; (defun declare-variable-types-globally (type vars)
;;   (proclaim `(type ,type ,@vars))
;;   type)
;; Once this form is executed, the dynamic variable *TOLERANCE*
;; must always contain a float.
;;(declare-variable-types-globally 'float '(*tolerance*)) ;; => FLOAT

(defvar *halt* (gensym))

;; ȫ�ֱ���*default-proc*: Ĭ�Ͻ��� 
;; pri��wait����nil 
(defvar *default-proc*
  (make-proc :state #'(lambda (x)
			(format t "~%>> ")
			;; Ĭ�Ͻ�����ʽ�ص�����eval�� �����������Ӧ�þ������⡣Ч�ʵ��£��������� 
			(princ (eval (read)))
			(pick-process))))

;;ʹ��һ������������ʵ��������
(defmacro fork (expr pri)
  `(prog1 ',expr
     ;; һ���½��̱����뵽��*procs*����
     (push (make-proc
	    :state #'(lambda (,(gensym))
		       ,expr
		       (pick-process))
	    :pri ,pri)
	   *procs*)))

(=defun foo (x)
  (format t "Foo was called with ~A.~%" x)
  (=values (1+ x)))
;; (foo 100) ;; => 101 
;; (macroexpand-1 '(fork (foo 100) 25))
;; => (PROG1 '(FOO 100)
;;   (PUSH (MAKE-PROC :STATE #'(LAMBDA (#:G3517) (FOO 100) (PICK-PROCESS))
;; 		   :PRI 25)
;; 	*PROCS*))

;;�����program��һ����̲������κ�ֵ��ֻӦ���� toplevel ������
(defmacro program (name args &body body)
  `(=defun ,name ,args
     (setq *procs* nil)
     ,@body
     ;; �ѿ�������(throw) ��*halt*�Ļ����ͻ���ֹ�������(program)����Ϊ���*halt*�Ǹ����ɷ��ţ��������û����õ� tag ��ͻ
     (catch *halt* (loop (pick-process)))))

;; (macroexpand-1 '(program two-foos (a b)
;; 		 (fork (foo a) 99)
;; 		 (fork (foo b) 99)))
;; (=DEFUN TWO-FOOS (A B)
;;   (SETQ *PROCS* NIL)
;;   (FORK (FOO A) 99)
;;   (FORK (FOO B) 99)
;;   (CATCH *HALT* (LOOP (PICK-PROCESS)))) 

;; �������
;; �ڿ��Լ���ִ�еĽ����У�ѡ�����ȼ���ߵ�һ����Ȼ��������
(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
      *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

;; ���һ������Ľ���û��wait������������ wait ���������棬��ô���ͱ���������
;; �����б��������еĽ����У�����������ȼ��ı�ѡ�С�ʤ���Ľ��̺����� wait ����(����еĻ�) ���ص�ֵ�����ظ� pick-process
;; ��ʤ�������Ǵ��ڣ���ΪĬ�Ͻ������ǿ��Ա�ִ��
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

;; ��ĳ�����̷Ž����̱�
(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
    (proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))


(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test) ;; ���Ժ���
    #'(lambda (,parm) ,@body))) ;; ִ�к���

(defmacro yield (&body body)
  `(arbitrator nil ;; waitΪ�գ���Զ����ִ��
	       #'(lambda (,(gensym)) ,@body))) ;; ִ�к���

(defun setpri (n) (setf (proc-pri *proc*) n))

;; �׳�*halt*
(defun halt (&optional val)
  (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
    (setq *procs* (apply #'delete obj *procs* args))
    (pick-process)))
