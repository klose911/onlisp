;; 23 使用ATN分析句子
;; 23.1 背景知识
;; 扩充转移网络(ATN)，是 Bill Woods在1970年提出的一种分析器
;; 1. 仅限用于语义上有限制的领域
;; 2. 不能给过于困难的输入 
;; 3. 仅仅适用于英语，或者其他单词的顺序决定其语法结构的语言
;; 4. 不要认为它们总是能正常工作

;; 23.2 形式化
;; 扩充转移网络(Augmented Transition Network)
;; 是指由有向路径连接起来的一组节点，从根本上可以把它看作一种流程图
;; 其中一个节点被指定为起始节点，而部分其他节点则被作为终结节点
;; 每条路径上都带有测试条件，只有对应的条件被满足的时候，状态才能经由这条路径转移到新的节点

;; 首先，输入是一个序列，并有一个指向当前单词的指针。根据路径进行状态转移会使指针相应地前进。使用转移网络分析句子的过程，就是找到从起始节点走到某个终止节点的路径的过程，在这个过程中，所有的转移条件都要满足

;; ATN 在这个模型的基础上另加入了两个特性：

;; 1. ATN带有寄存器, 寄存器是有名字的slot，可以被用来保存分析过程中所需的有关信息
;;    转移路径除了能进行条件判断之外，还会设置和修改寄存器中的内容。
;; 2. ATN 的结构可以是递归的
;;    转移路径可以这样要求：如果要通过这条路径，分析过程必须能通过某个子网络, 而终结节点则使用寄存器中累积得到信息来建立列表结构并返回它

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

;; 当 ATN 分析输入序列 (spot runs) 时，它是如何工作的呢？

;; 第一个节点有一条出路径(outgoingarc)，或者说一条类型路径(cat)，这条路径指向节点s2
;; 这事实上：如果当前单词是个名词的话，你就可以通过我；如果你通过我的话，你必须把当前单词(即*)保存在subj 寄存器中。因而，当离开这个节点时，subj 的内容就变成了spot

;; 总有个指针指向当前的单词。在开始的时候，它指向句子的第一个单词。在经过cat 路径的时候，指针会往前移动一个单词。因此，在我们到达s2 节点的时候，当前节点会变成第二个单词，即runs
;; 第二条路径线和第一条一样，不同之处在于它要求的是个动词。它发现了runs ，并把它保存在寄存器v 里面，然后状态就走到了s3

;; 在最后一个节点s3 上，只有一个pop 路径(或称为终止路径)
;; 由于我们正好在把输入序列读完的时候通过了pop 路径，所以我们进行的句子分析是成功的

;; pop 路径返回的是一个反引用表达式：
;; (sentence (subject spot)
;;   (verb runs))

;; 递归 
;; 用来分析英语的 ATN，如果规模适中的话，那么它会有一个用来分析句子的主网络，以及用来分析名词短语、介词短语，以及修饰词组等语法元素的多个子网络
;; 介词短语也是有可能含有名词短语的，并且这种结构可能会无穷无尽地延续下去
;; 要处理下面这种结构的句子，必须要能支持递归：
;;        "the key on the table in the hall of the house on the hill"

;; 23.3 非确定性
;; ATN应该能分析祈使句也能分析陈述句。所以第一个节点要有向外的 cat 路径，与名词(用于陈述句)和动词(用于祈使句)
;; 实际上分析器是无法预见未来的。它只是在无路可走，或者读完了输入还没能结束分析时，通过回溯的方式来表现出老是猜中的表象, 不过所有这些回溯的机制是自动嵌入在ATN编译器产生的代码里面的
;; ATN基于深度搜索，出路径被选中的顺序就是它们当初被定义的顺序。使用这样的设计，程序员就可以根据优先级来排列转换路径线的定义了

;; 23.4 一个ATN 编译器
;; 一个基于ATN的分析器由三个部分组成：ATN本身，用来遍历这个ATN的解释器，还有一个可以用于查询的词典
;; 使用一个比较初级的手工编制的词典
;; ATN本身：直接翻译成Lisp代码
;; ATN编译器：能把整个的ATN变成对应的代码
;; 使用函数作为表达的方式：节点会成为函数，而转换路径则会变成函数里的代码块

(load "continuation.lisp")
(load "amb.lisp")

;; defnode宏被用来定义节点, 就是一个 choose
;; 节点函数有两个参数，分别是 pos 和 regs：
;; pos: 是当前的输入指针(一个整数)
;; regs: 是当前的寄存器组(为一个关联表的列表) 
(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

;; (macroexpand-1 '(defnode foo
;;   <arc 1>
;;   <arc 2>)) 
;; (=DEFUN FOO (POS REGS)
;;   (CHOOSE <ARC 1>
;; 	  <ARC 2>))

;; 路径转换
(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos) ;; 
    (fail)
    (let ((* (nth pos *sent*))) ;; 符号*将会被绑定到当前的输入单词上
      (if (member ',cat (types *)) ;;要求当前的输入单词在语法上属于某个类型
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

;; down定义的push路径，要求对子网络的调用能成功返回
;; sub:  子网络目标节点
;; next: 当前网络的下个节点
(defmacro down (sub next &rest cmds)
  ;; 虽然为cat路径生成的代码只是调用了网络中的下一个节点，但是为push路径生成的代码使用的是=bind
  `(=bind (* pos regs)
       (,sub pos (cons nil regs)) 
     ;; 在push 路径中，*则是被绑定到从子网络返回的表达式
     ;; regs被传入子网络前，一组新的空寄存器(nil)被cons到它的前面
     (,next pos ,(compile-cmds cmds))))
;; (macroexpand-1 '(down np s/subj
;; 		 (setr mood 'decl)
;; 		 (setr subj *)))  
;; (=BIND (* POS REGS)
;;     (NP POS (CONS NIL REGS))
;;   (S/SUBJ POS
;; 	  (SETR MOOD 'DECL 
;; 		(SETR SUBJ * REGS))))

;; jump路径就像发生了短路一样
;; 分析器直接跳到了目标节点，不需要进行条件测试，同时输入指针没有向前移动。
(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

;; pop路径由up定义
(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     ;;pop路径的=values "返回" 的是最近的一个push路径的=bind
     ;;=bind的函数体已经被包在一个续延里了，并且被作为参数顺着之后的转移路径一直传下去，直到pop 路径的=values 把"返回" 值作为参数调用这个续延
    (=values ,expr pos (cdr regs))))


;; 寄存器组是用关联表来表示的
;; ATN所使用的并不是寄存器组，而是一系列寄存器组
;; 当分析器进入一个子网络时，它获得了一组新的空寄存器，这组寄存器被压在了已有寄存器组的上面

;; 寄存器不需要事先声明。不管传给set-register的是什么名字，它都会用这个名字新建一个寄存器!!!
(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
	 (cdr ,regs)))
;; (macroexpand-1 ' (SET-REGISTER 'SUBJ (LIST *)
;; 			       REGS))
;; (CONS (CONS (CONS 'SUBJ
;; 		  (LIST *))
;; 	    (CAR REGS))
;;       (CDR REGS)) 

;; getr读一个寄存器
(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
    (if (cdr result) result (car result))))

;; setr设置寄存器
(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

;; (macroexpand-1 '(setr mood 'decl (setr subj * regs))) 
;; (SET-REGISTER 'MOOD (LIST 'DECL)
;; 	      (SETR SUBJ * REGS))

;; (macroexpand-1 '(setr subj * regs))
;; (SET-REGISTER 'SUBJ (LIST *) REGS)

;; pushr 把一个值加入寄存器
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

;; push,cat和jump路径都可以包含表达式体。这些表达式只不过会是一些setr罢了
;; 通过对它们的表达式体调用compile-cmds ，转移路径的展开函数会把一系列setr串在一起，成为一个单独的表达式
(defun compile-cmds (cmds)
  (if (null cmds)
    'regs
    `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

;; (compile-cmds '((setr a b) (setr c d)))
;; (SETR A B (SETR C D REGS))

;; (compile-cmds '((setr mood 'decl) (setr subj *)))  
;; => (SETR MOOD 'DECL (SETR SUBJ * REGS)) 
