(defpackage #:alg-9
  (:use :cl))

(in-package #:alg-9)


;;;; 9 中位数和顺序统计量

; 一个中位数(median)是它所属集合的`中点元素`
; 集合数量n位奇数时，中位数唯一
; 为偶数时，存在两个中位数(上/下中位数)

;;;; 9.1 最大值和最小值

; 在一个有n个元素的集合中，确定最值元素时，很容易给出需要n-1比较这个上界
; 依次遍历每个元素，纪录当前最值，比如求最小值：
; MINIMUM(A)
;     min = A[0]
;     for i = 1 to A.length - 1
;         if min > A[i]
;             min = A[i]
;     return min
; 
; 从执行次数来看，算法MINIMUM是最优的

;;; 同时找到最大值和最小值

; 一般情况，同时找到最大和最小值，只需要分别独立找出最小值和最大值
; 这各需要n-1次比较，共需2n-2次比较

; 实际上，我们只需3 * (floor n 2)[简化成n*3/2]次比较就可以实现：
; 纪录已知的最大最小值，对输入元素成对进行处理，然后将较小的与当前最小值比较
; 较大的与当前最大值比较，这样，每对元素就需要3次比较


;;;; 9.2 期望为线性时间的选择算法

; 一般选择问题看起来要比找最值这样的简单问题更难
; 但这两个问题的渐进运行时间是相同的：O(n)
; 下面介绍一种解决选择问题的分治算法
; RANDOMIZED-SELECT算法是以快速排序为模型的
; 与快速排序一样，将输入数组进行递归划分
; 但与快速排序不同的是，快速排序会递归的处理两边的划分
; 而RANDOMIZED-SELECT只处理划分的一边
; 这一差异会在性能分析中体现出来：
; 期望运行时间 1 快速排序O(nlgn) 2 RANDOMIZED-SELECT O(n)
; 假设输入数据都是互异的，以下是RANDOMIZED-SELECT代码
; 返回数组A[l...r]中第i小的元素

(defun randomized-select (arr l r i)
  "
RANDOMIZED-SELECT(A, l, r, i)
    if l == r
        return A[r]
    q = RANDOMIZED-PARTITION(A, l, r)
    k = q - l + 1
    if i == k
        return A[q]
    else if i < k
        return RANDOMIZED-SELECT(A, l, q-1, i)
    else
        return RANDOMIZED-SELECT(A, q+1, r, i-k)
"
  (labels ((exchange (arr i j)
	     "exchange arr[i] with arr[j]"
	     (let ((dummy nil))
	       (setf dummy (aref arr i))
	       (setf (aref arr i) (aref arr j))
	       (setf (aref arr j) dummy)
	       arr))
	   (my-random (l r)
	     "随机返回区间[l r]中的一个值"
	     (+ (random (- r l -1)) l))
	   (partition (arr l r)
	     (let ((k (aref arr r))
		   (i l))
	       (do ((j l (+ j 1)))
		   ((> j (- r 1)))
		 (if (< (aref arr j) k)
		     (progn
		       (exchange arr i j)
		       (setf i (1+ i))))
		 (exchange arr i r))
	       (values i arr)))
	   (randomized-partion (arr l r)
	     (let ((m (my-random l r)))
	       (exchange arr m r)
	       (partition arr l r))))
					;(exchange arr 0 1) ;test ok
					(format t "random number: ~A~%" (my-random 1 2))
					;(partition (vector 4 3 1 2) 0 3)
					;(randomized-partion (vector 1 4 2 3) 0 3)
    (if (= l r)
	(values l (aref arr l) arr)
	(let* ((m (randomized-partion arr l r))
	       (k (- m l -1)))
	  (cond ((= i k) (values m (aref arr m) arr))
		((< i k) (randomized-select arr l (- m 1) i))
		(t (randomized-select arr (+ m 1) r (- i k))))))))
