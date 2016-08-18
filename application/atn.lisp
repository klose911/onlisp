;; 23 ʹ��ATN��������
;; 23.1 ����֪ʶ
;; ����ת������(ATN)���� Bill Woods��1970�������һ�ַ�����
;; 1. �������������������Ƶ�����
;; 2. ���ܸ��������ѵ����� 
;; 3. ����������Ӣ������������ʵ�˳��������﷨�ṹ������
;; 4. ��Ҫ��Ϊ������������������

;; 23.2 ��ʽ��
;; ����ת������(Augmented Transition Network)
;; ��ָ������·������������һ��ڵ㣬�Ӹ����Ͽ��԰�������һ������ͼ
;; ����һ���ڵ㱻ָ��Ϊ��ʼ�ڵ㣬�����������ڵ�����Ϊ�ս�ڵ�
;; ÿ��·���϶����в���������ֻ�ж�Ӧ�������������ʱ��״̬���ܾ�������·��ת�Ƶ��µĽڵ�

;; ���ȣ�������һ�����У�����һ��ָ��ǰ���ʵ�ָ�롣����·������״̬ת�ƻ�ʹָ����Ӧ��ǰ����ʹ��ת������������ӵĹ��̣������ҵ�����ʼ�ڵ��ߵ�ĳ����ֹ�ڵ��·���Ĺ��̣�����������У����е�ת��������Ҫ����

;; ATN �����ģ�͵Ļ�������������������ԣ�

;; 1. ATN���мĴ���, �Ĵ����������ֵ�slot�����Ա������������������������й���Ϣ
;;    ת��·�������ܽ��������ж�֮�⣬�������ú��޸ļĴ����е����ݡ�
;; 2. ATN �Ľṹ�����ǵݹ��
;;    ת��·����������Ҫ�����Ҫͨ������·�����������̱�����ͨ��ĳ��������, ���ս�ڵ���ʹ�üĴ������ۻ��õ���Ϣ�������б�ṹ��������

;; noun verb pop

;; (defnode s
;;   (cat noun s2
;;     (setr subj *)))

;; (defnode s2
;;   (cat verb s3
;;     (setr v *)))

;; (defnode s3
;;   (up `(sentence
;;       (subject ,(getr subj))
;;       (verb ,(getr v)))))

;; �� ATN ������������ (spot runs) ʱ��������ι������أ�

;; ��һ���ڵ���һ����·��(outgoingarc)������˵һ������·��(cat)������·��ָ��ڵ�s2
;; ����ʵ�ϣ������ǰ�����Ǹ����ʵĻ�����Ϳ���ͨ���ң������ͨ���ҵĻ��������ѵ�ǰ����(��*)������subj �Ĵ����С���������뿪����ڵ�ʱ��subj �����ݾͱ����spot

;; ���и�ָ��ָ��ǰ�ĵ��ʡ��ڿ�ʼ��ʱ����ָ����ӵĵ�һ�����ʡ��ھ���cat ·����ʱ��ָ�����ǰ�ƶ�һ�����ʡ���ˣ������ǵ���s2 �ڵ��ʱ�򣬵�ǰ�ڵ���ɵڶ������ʣ���runs
;; �ڶ���·���ߺ͵�һ��һ������֮ͬ��������Ҫ����Ǹ����ʡ���������runs �������������ڼĴ���v ���棬Ȼ��״̬���ߵ���s3

;; �����һ���ڵ�s3 �ϣ�ֻ��һ��pop ·��(���Ϊ��ֹ·��)
;; �������������ڰ��������ж����ʱ��ͨ����pop ·�����������ǽ��еľ��ӷ����ǳɹ���

;; pop ·�����ص���һ�������ñ��ʽ��
;; (sentence (subject spot)
;;   (verb runs))

;; �ݹ� 
;; ��������Ӣ��� ATN�������ģ���еĻ�����ô������һ�������������ӵ������磬�Լ������������ʶ����ʶ���Լ����δ�����﷨Ԫ�صĶ��������
;; ��ʶ���Ҳ���п��ܺ������ʶ���ģ��������ֽṹ���ܻ������޾���������ȥ
;; Ҫ�����������ֽṹ�ľ��ӣ�����Ҫ��֧�ֵݹ飺
;;        "the key on the table in the hall of the house on the hill"

;; 23.3 ��ȷ����
;; ATNӦ���ܷ�����ʹ��Ҳ�ܷ��������䡣���Ե�һ���ڵ�Ҫ������� cat ·����������(���ڳ�����)�Ͷ���(������ʹ��)
;; ʵ���Ϸ��������޷�Ԥ��δ���ġ���ֻ������·���ߣ����߶��������뻹û�ܽ�������ʱ��ͨ�����ݵķ�ʽ�����ֳ����ǲ��еı���, ����������Щ���ݵĻ������Զ�Ƕ����ATN�����������Ĵ��������
;; ATN���������������·����ѡ�е�˳��������ǵ����������˳��ʹ����������ƣ�����Ա�Ϳ��Ը������ȼ�������ת��·���ߵĶ�����

;; 23.4 һ��ATN ������
;; һ������ATN�ķ�����������������ɣ�ATN���������������ATN�Ľ�����������һ���������ڲ�ѯ�Ĵʵ�
;; ʹ��һ���Ƚϳ������ֹ����ƵĴʵ�
;; ATN����ֱ�ӷ����Lisp����
;; ATN���������ܰ�������ATN��ɶ�Ӧ�Ĵ���
;; ʹ�ú�����Ϊ���ķ�ʽ���ڵ���Ϊ��������ת��·������ɺ�����Ĵ����

;;(cd "/home/klose/Documents/programming/lisp/onlisp/application") 
(load "continuation.lisp")
(load "amb.lisp")

;; �Ĵ��������ù���������ʾ��
;; ATN��ʹ�õĲ����ǼĴ����飬����һϵ�мĴ�����
;; ������������һ��������ʱ���������һ���µĿռĴ���������Ĵ�����ѹ�������мĴ����������

;; �Ĵ�������Ҫ�������������ܴ���set-register����ʲô���֣�����������������½�һ���Ĵ���!!!
(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
	 (cdr ,regs)))
;;(set-register 'mood '(fantasy) regs) ;; (((MOOD FANTASY))) 
;; (macroexpand-1 ' (set-register 'mood '(fantasy) 
;; 			       regs))  
;; (CONS (CONS
;;        (CONS 'MOOD '(FANTASY))
;;        (CAR REGS))
;;       (CDR REGS)) 


;; getr��һ���Ĵ���
(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     (if (cdr result) result (car result))))

;; (macroexpand-1 '(getr mood))
;; (LET ((RESULT
;;        (CDR (ASSOC 'MOOD (CAR REGS)))))
;;   (IF (CDR RESULT)
;;       RESULT
;;       (CAR RESULT))) 

;; setr���üĴ���
(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

;; (macroexpand-1 '(setr mood 'decl regs))  
;; (SET-REGISTER 'MOOD (LIST 'DECL) REGS)

;; pushr��һ��ֵ����Ĵ���
(defmacro pushr (key val regs)
  `(set-register ',key
		 (cons ,val (cdr (assoc ',key (car ,regs))))
		 ,regs))

;; (macroexpand-1 '(pushr mood 'decl2 regs))
;; (SET-REGISTER 'MOOD (CONS 'DECL2 (CDR (ASSOC 'MOOD (CAR REGS)))) REGS) 

;; (setq klose-register (set-register 'klose '(good) regs))
;;  (((KLOSE GOOD))) 
;; (getr klose  klose-register) ;; good 
;; (setq klose-register (setr mom 'better klose-register))
;;  (((MOM BETTER) (KLOSE GOOD))) 
;; (getr mom klose-register) ;; better 
;; (setq klose-register (pushr mom 'best klose-register))
;;  (((MOM BEST BETTER) (MOM BETTER) (KLOSE GOOD)))
;; (getr mom klose-register)  ;; (best better)

;; (setq test-register (setr Mood 'Decl (setr Subj 'Xman regs))) 
;; test-register ;; (((MOOD DECL) (SUBJ XMAN)))

;; push,cat��jump·�������԰������ʽ�塣��Щ���ʽֻ��������һЩsetr����
;; ͨ�������ǵı��ʽ�����compile-cmds ��ת��·����չ���������һϵ��setr����һ�𣬳�Ϊһ�������ı��ʽ
(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

;; (compile-cmds '((setr a b) (setr c d)))
;; (SETR A B (SETR C D REGS))

;; (compile-cmds '((setr mood 'decl) (setr subj *)))
;; => (SETR MOOD 'DECL (SETR SUBJ * REGS)) 

;; (SETR MOOD 'DECL (SETR SUBJ * REGS))
;; (((MOOD DECL) (SUBJ (NIL)))) 
;; (macroexpand-1 '(SETR SUBJ * REGS))
;; (macroexpand-1 '(SET-REGISTER 'SUBJ (LIST *) REGS))
;; (CONS (CONS (CONS 'SUBJ (LIST *)) (CAR REGS)) (CDR REGS))

;; defnode�걻��������ڵ�, ����һ�� choose
;; �ڵ㺯���������������ֱ��� pos �� regs��
;; pos: �ǵ�ǰ�����ھ����е�λ��(һ������)
;; regs: �ǵ�ǰ�ļĴ�����(Ϊһ����������б�) 
(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

;; (macroexpand-1 '(defnode foo
;;   <arc 1>
;;   <arc 2>)) 
;; (=DEFUN FOO (POS REGS)
;;   (CHOOSE <ARC 1>
;; 	  <ARC 2>))

;; ·��ת��
(defmacro cat (category next &rest cmds)
  `(if (= (length *sent*) pos) ;; 
       (fail)
       (let ((* (nth pos *sent*))) ;; ����*���ᱻ�󶨵���ǰ�����뵥����
	 (if (member ',category (types *)) ;;Ҫ��ǰ�����뵥�����﷨������ĳ������
	     (,next (1+ pos) ,(compile-cmds cmds)) 
	     (fail)))))
;; (macroexpand-1 '(cat v v
;; 		 (setr mood 'imp) 
;; 		 (setr subj '(np (pron you)))
;; 		 (setr aux nil) 
;; 		 (setr v *)))

;; (IF (= (LENGTH *SENT*) POS)
;;     (FAIL)
;;     (LET ((* (NTH POS *SENT*)))
;;       (IF (MEMBER 'V (TYPES *))
;; 	  (V (1+ POS)
;; 	     (SETR MOOD 'IMP
;; 		   (SETR SUBJ '(NP (PRON YOU))
;; 			 (SETR AUX NIL
;; 			       (SETR V * REGS)))))
;; 	  (FAIL))))

;; down�����push·����Ҫ���������ĵ����ܳɹ�����
;; sub:  ������Ŀ��ڵ�
;; next: ��ǰ������¸��ڵ�
(defmacro down (sub next &rest cmds)
  ;; ��ȻΪcat·�����ɵĴ���ֻ�ǵ����������е���һ���ڵ㣬����Ϊpush·�����ɵĴ���ʹ�õ���=bind
  `(=bind (* pos regs) ;; *��Ϊ��ǰ�������һ���ڵ㣬pos������pos�� regs����Ϊ,(compile-cmds cmds)
       ;;��������ĵ��ã����ý����󷵻ظ���ǰ����
       (,sub pos (cons nil regs)  ;; regs������������ǰ��һ���µĿռĴ���(nil)��cons������ǰ��
	     ) 
     ;; ��push·���У�*���Ǳ��󶨵��������緵�صı��ʽ
     (,next pos ,(compile-cmds cmds)))) 

;; (macroexpand-1 '(down np s/subj
;; 		 (setr mood 'decl)
;; 		 (setr subj *)))

;; (=BIND (* POS REGS)
;;     (NP POS (CONS NIL REGS))
;;   (S/SUBJ POS (SETR MOOD 'DECL (SETR SUBJ * REGS))))

;; (macroexpand-1 '(=BIND (* POS REGS)
;;     (NP POS (CONS NIL REGS))
;;   (S/SUBJ POS
;; 	  (SETR MOOD 'DECL 
;; 	   (SETR SUBJ * REGS)))))
;; (LET ((*CONT*
;;        #'(LAMBDA (* POS REGS) 
;; 	   (S/SUBJ POS (SETR MOOD 'DECL (SETR SUBJ * REGS))))))
;;   (NP POS (CONS NIL REGS))) 

;; (LET ((*CONT*
;;        #'(LAMBDA (* POS REGS) 
;; 	   (S/SUBJ POS (SETR MOOD 'DECL (SETR SUBJ * REGS))))))
;;   (FUNCALL *CONT* NP POS (CONS NIL REGS)))  


;; jump·���������˶�·һ��
;; ������ֱ��������Ŀ��ڵ㣬����Ҫ�����������ԣ�ͬʱ����ָ��û����ǰ�ƶ���
(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

;; pop·����up����
(defmacro up (expr)
  `(let ((* (nth pos *sent*))) ;; ��ǰ����
     (=values ,expr pos (cdr regs))))

;; (macroexpand-1 '(up `(sentence
;;       (subject ,(getr subj))
;; 	(verb ,(getr v))))) 
;; (LET ((* (NTH POS *SENT*)))
;;   (=VALUES `(SENTENCE (SUBJECT ,(GETR SUBJ)) (VERB ,(GETR V)))
;; 	   POS (CDR REGS)))

(defmacro my-with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;; ATN�������
;; ��ʼ�ڵ�����֡�һ����Ҫ�����ı��ʽ���Լ�һ��������
;; ���������with-parsesӦ����δ����صķ������
(defmacro with-parses (node sent &body body)
  (my-with-gensyms (pos regs)
    `(progn
       (setq *sent* ,sent) ;; Ҫ�����ľ���
       (setq *paths* nil)  ;; ��������
       ;; ÿ�γɹ��ķ����������������with-parses���ʽ�еĴ������һ����ֵ
       ;; �ڴ������У�����parse����󶨵���ǰ�ķ��������
       ;; with-parses���ʽ�᷵��@ ����Ϊ������fail����;ĩ·ʱ�ķ���ֵ
       (=bind (parse ,pos ,regs) (,node 0 '(nil))
	 (if (= ,pos (length *sent*))
	     (progn ,@body (fail))
	     (fail))))))  

;; �򵥵Ĵʵ�
(defun types (w)
  (cdr (assoc w '((spot noun) (runs verb))))) 

;; �����ǰ�ڵ������ʣ���ת�Ƶ�S2�ڵ㣬�����浱ǰ�Ĵ���Ϊ(SUBJ ��ǰ����)������ӡ 
(defnode s
    (cat noun s2 
	 (setr subj *)))

;; (macroexpand-1 '(defnode s
;;     (cat noun s2
;;      (setr subj *))))
;; (=DEFUN S (POS REGS)
;;   (CHOOSE (CAT NOUN S2
;; 	       (SETR SUBJ *))))

;; (macroexpand-1 '(=DEFUN S (POS REGS)
;;   (CHOOSE (CAT NOUN S2
;; 	   (SETR SUBJ *)))))
;; (PROGN (DEFMACRO S (POS REGS)
;; 	 (LIST '=S '*CONT* POS REGS))
;;        (DEFUN =S (*CONT* POS REGS)
;; 	 (CHOOSE (CAT NOUN S2
;; 		      (SETR SUBJ *)))))

;; (macroexpand-1 '(CHOOSE (CAT NOUN S2
;; 		      (SETR SUBJ *))))  
;; (IF (= (LENGTH *SENT*) POS)
;;     (FAIL)
;;     (LET ((* (NTH POS *SENT*)))
;;       (IF (MEMBER 'NOUN (TYPES *))
;; 	  (S2 (1+ POS)
;; 	      (SETR SUBJ * REGS))
;; 	  (FAIL))))

;; (macroexpand-1 '(SETR SUBJ * REGS)) 
;; (SET-REGISTER 'SUBJ (LIST *) REGS) 

;; (macroexpand-1 '(SET-REGISTER 'SUBJ (LIST *) REGS))
;; (CONS (CONS (CONS 'SUBJ (LIST *)) (CAR REGS)) (CDR REGS))

;; (PROGN (DEFMACRO S (POS REGS)
;; 	 (LIST '=S '*CONT* POS REGS))
;;        (DEFUN =S (*CONT* POS REGS)
;; 	 (CHOOSE
;; 	  (IF (= (LENGTH *SENT*) POS)
;; 	      (FAIL)
;; 	      (LET ((* (NTH POS *SENT*)))
;; 		(IF (MEMBER 'NOUN (TYPES *))
;; 		    (S2 (1+ POS)
;; 		        (CONS (CONS
;; 			       (CONS 'SUBJ (LIST *))
;; 			       (CAR REGS))
;; 			      (CDR REGS)))
;; 		    (FAIL)))))))

(defnode s2
    (cat verb s3
	 (setr v *)))

;; pop��㣬��ӡ���(SENTENCE (SUBJECT 'SUBJ'�Ĵ���������) (VERB 'V'�Ĵ�������ͬ)) 
(defnode s3
    (up `(sentence
	  (subject ,(getr subj))
	  (verb ,(getr v)))))

;; (macroexpand-1 '(up `(sentence
;; 	  (subject ,(getr subj))
;; 	  (verb ,(getr v)))))
;; (LET ((* (NTH POS *SENT*)))
;;   (=VALUES `(SENTENCE (SUBJECT ,(GETR SUBJ))
;; 		      (VERB ,(GETR V)))
;; 	   POS (CDR REGS)))

;; (macroexpand-1 '(=VALUES `(SENTENCE (SUBJECT ,(GETR SUBJ))
;; 		      (VERB ,(GETR V)))
;; 	   POS (CDR REGS))) 
;; (FUNCALL *CONT* `(SENTENCE (SUBJECT ,(GETR SUBJ))
;; 			   (VERB ,(GETR V)))
;; 	 POS (CDR REGS))

(with-parses s '(spot runs)
  (format t "Parsing: ~A~%" parse)) ;; Parsing: (SENTENCE (SUBJECT SPOT) (VERB RUNS)) 

;; (macroexpand-1 '(with-parses s '(spot runs)
;; 		 (format t "Parsing: ~A~%" parse)))
;; (PROGN (SETQ *SENT* '(SPOT RUNS))
;;        (SETQ *PATHS* NIL)
;;  (=BIND (PARSE #:G3517 #:G3518) (S 0 '(NIL))
;;    (IF (= #:G3517 (LENGTH *SENT*))
;;        (PROGN (FORMAT T "Parsing: ~A~%" PARSE)
;; 	      (FAIL))
;;        (FAIL))))

;; (macroexpand-1 '(=BIND (PARSE #:G3517 #:G3518) (S 0 '(NIL))
;;    (IF (= #:G3517 (LENGTH *SENT*))
;;        (PROGN (FORMAT T "Parsing: ~A~%" PARSE)
;; 	      (FAIL))
;;        (FAIL))))
;; (LET
;;     ((*CONT*
;;       #'(LAMBDA (PARSE #:G3517 #:G3518)
;; 	  (IF (= #:G3517 (LENGTH *SENT*))
;; 	      (PROGN (FORMAT T "Parsing: ~A~%" PARSE)
;; 		     (FAIL))
;; 	      (FAIL)))))
;;   (S 0 '(NIL)))

;; 23.5 ATN�ĸ�������
