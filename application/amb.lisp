;; 22 非确定性
;; 22.1 概念 
;; choose函数，接受一个列表，并返回一个元素 
;; 如果这个元素被选中，那么这个计算过程就会因为它而导致有一组可能的未来情况与之对应 

;; (let ((x (choose '(1 2 3))))
;;   (if (odd? x)
;;     (+ x 1)
;;     x))  
;; 如果 choose 返回 1，那么这个运算过程将会经过 if 的 then 语句，然后返回 2
;; 如果 choose 返回 2 ，那么这个运算过程将会经过 if 的 else 语句，然后返回 2
;; 如果 choose 返回 3 ，那么这个运算过程将会经过 if 的 then 语句，然后返回 4 

;; 如果将来的可能性中存在某种情况，在这种情况下没有调用 fail ，那么 choose 将可以返回这个选择的结果
;; (let ((x (choose '(1 2))))
;; (if (odd? x)
;;   (fail)
;;   x))
;; 这个choose只会选择2， 这个表达式是确定性的：它总是返回 2  

;; (let ((x (choose '(1 2))))
;;   (if (odd? x)
;;     (let ((y (choose '(a b))))
;;       (if (eq? y 'a)
;;         (fail)
;;         y))
;;     x))
;; 这个表达式总的来说，要么返回 b ，要么返回 2  

;; (let ((x (choose '(1 2))))
;;   (if (odd? x)
;;     (choose '()) ;; 如果要在零个选项里作选择，那么这个 choose 就等价于 fail 
;;     x))

;; 祖上是不是有人名叫 Igor
;; function Ig(n)
;;   if name(n) = 'Igor'
;;     then return n
;;   else if parrents(n)
;;     then return Ig(choose(parents(n))
;;   else fail

;; fail 操作符被用来对 choose 的返回值施加影响
;; 如果我们碰到一个 fail ，那么可以推断 choose 在此之前肯定做了错误的选择!!!

