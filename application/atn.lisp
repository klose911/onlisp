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

(load "continuation.lisp")
(load "amb.lisp")

;; defnode�걻��������ڵ�, ����һ�� choose
;; �ڵ㺯���������������ֱ��� pos �� regs��
;; pos: �ǵ�ǰ������ָ��(һ������)
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
(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos) ;; 
    (fail)
    (let ((* (nth pos *sent*))) ;; ����*���ᱻ�󶨵���ǰ�����뵥����
      (if (member ',cat (types *)) ;;Ҫ��ǰ�����뵥�����﷨������ĳ������
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
  `(=bind (* pos regs)
       (,sub pos (cons nil regs)) 
     ;; ��push ·���У�*���Ǳ��󶨵��������緵�صı��ʽ
     ;; regs������������ǰ��һ���µĿռĴ���(nil)��cons������ǰ��
     (,next pos ,(compile-cmds cmds))))
;; (macroexpand-1 '(down np s/subj
;; 		 (setr mood 'decl)
;; 		 (setr subj *)))  
;; (=BIND (* POS REGS)
;;     (NP POS (CONS NIL REGS))
;;   (S/SUBJ POS
;; 	  (SETR MOOD 'DECL 
;; 		(SETR SUBJ * REGS))))

;; jump·���������˶�·һ��
;; ������ֱ��������Ŀ��ڵ㣬����Ҫ�����������ԣ�ͬʱ����ָ��û����ǰ�ƶ���
(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

;; pop·����up����
(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     ;;pop·����=values "����" ���������һ��push·����=bind
     ;;=bind�ĺ������Ѿ�������һ���������ˣ����ұ���Ϊ����˳��֮���ת��·��һֱ����ȥ��ֱ��pop ·����=values ��"����" ֵ��Ϊ���������������
    (=values ,expr pos (cdr regs))))


;; �Ĵ��������ù���������ʾ��
;; ATN��ʹ�õĲ����ǼĴ����飬����һϵ�мĴ�����
;; ������������һ��������ʱ���������һ���µĿռĴ���������Ĵ�����ѹ�������мĴ����������

;; �Ĵ�������Ҫ�������������ܴ���set-register����ʲô���֣�����������������½�һ���Ĵ���!!!
(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
	 (cdr ,regs)))
;; (macroexpand-1 ' (SET-REGISTER 'SUBJ (LIST *)
;; 			       REGS))
;; (CONS (CONS (CONS 'SUBJ
;; 		  (LIST *))
;; 	    (CAR REGS))
;;       (CDR REGS)) 

;; getr��һ���Ĵ���
(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
    (if (cdr result) result (car result))))

;; setr���üĴ���
(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

;; (macroexpand-1 '(setr mood 'decl (setr subj * regs))) 
;; (SET-REGISTER 'MOOD (LIST 'DECL)
;; 	      (SETR SUBJ * REGS))

;; (macroexpand-1 '(setr subj * regs))
;; (SET-REGISTER 'SUBJ (LIST *) REGS)

;; pushr ��һ��ֵ����Ĵ���
(defmacro pushr (key val regs)
  `(set-register ',key
    (cons ,val (cdr (assoc ',key (car ,regs))))
    ,regs))

(defvar regs nil)
(setq klose-register (set-register 'klose '(good) regs))
;; (((KLOSE GOOD))) 
(getr klose  klose-register) ;; good 
(setq klose-register (setr mom 'better klose-register))
;; (((MOM BETTER) (KLOSE GOOD))) 
(getr mom klose-register) ;; better 
(setq klose-register (pushr mom 'best klose-register))
;; (((MOM BEST BETTER) (MOM BETTER) (KLOSE GOOD)))
(getr mom klose-register)  ;; (best better)

(setq test-register (setr Mood 'Decl (setr Subj 'Xman regs))) 
test-register ;; (((MOOD DECL) (SUBJ XMAN)))

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
