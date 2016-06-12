(defpackage #:alg-2
  (:use :cl))

(in-package #:alg-2)

;;;; 2.1 插入排序
;;;; 实现升序或降序排列
(defun insertion-sort (arr symbol)
  "
arr is of type LIST
symbol must be `>` or `<`
                                      执行时间          执行次数
for j = 1 until arr.length               c1               n
    key = arr[j]                      c2               n - 1
    i = j - 1                         c4               n - 1

                                                       n
    while i >= 0 and arr[i] > key     c5               Σ Tj
                                                       j=1
                                                       n
        arr[i + 1] = a[i]             c6               Σ (Tj - 1)
                                                       j=1
                                                       n
        i = i - 1                     c7               Σ (Tj - 1)
                                                       j=1
    a[i + 1] = key                    c8               n - 1
"
  (let ((len (length arr)))
    (do ((j 1 (1+ j)))
	((>= j len) arr)
      (let ((key (nth j arr)))
	(do ((i (- j 1) (1- i)))
	    ((not (and (>= i 0) (funcall symbol (nth i arr) key))) (setf (nth (+ i 1) arr) key))
	  (setf (nth (+ i 1) arr) (nth i arr)))))))

;;;; 2.2 分析算法

; 分析算法意味着需要预测算法需要的资源
; 通常包括像内存，通信带宽，或计算机硬件这类资源
; 但我们更想度量的是计算时间

; 插入算法分析
; 如insertion-sort伪代码中，可以计算出运行时间T(n) (n = a.length)
; 所以：
; T(n) = c1 * n
;      + c2 * (n - 1)
;      + c4 * (n - 1)
; 
;             n
;      + c5 * Σ Tj
;             j=1
;             n
;      + c6 * Σ (Tj - 1)
;             j=1
;             n
;      + c7 * Σ (Tj - 1)
;             j=1
; 
;      + c8 * (n - 1)
; 如果是最佳情况，即正向排序
; T(n) = c1 * n + c2 * (n - 1) + c4 * (n - 1) + c5 * (n - 1) + c8 * (n - 1)
;      = (c1 + c2 + c4 + c5 + c8) * n - (c2 + c4 + c5 + c8)
; 可表示为 an + b，a b依赖ci，因此，T(n)是n的线性函数
; 如果是最坏情况，即反向排序
; 此时：
; n
; Σ Tj = n(n+1)/2 - 1
; j=1
; 
; n
; Σ (Tj - 1) = n(n-1)/2
; j=1
;
; T(n) =
;      c1 * n + c2 * (n - 1) + c4 * (n - 1) + c5 * (n(n+1)/2 - 1) + c6 * (n(n-1)/2) + c7 * (n(n-1)/2) + c8 * (n-1)
; 可表示为 an^2 + bn + c，a b c依赖ci，所以，T(n)是n的二次函数
; 
; 一般情况下，我们把最坏运行时间作为一个算法的时间复杂度O
; 插入排序时间时间复杂度 T(n) = an^2 + bn + c
; 我们真正感兴趣的是运行时间的增长率或增长量级
; 当n很大时，最高阶项具有最大影响，可以忽略常数项和低阶项
; 最终，插入算法的时间复杂度为O(n^2)
; 一个算法是否更有效率，通常它具有更低的增长量级

;;; 数学基础知识
; 求和符号(Σ, sigma), 用于求多项数的和
; 使用方法:
;   n
;  Σ Ak = A1 + A2 + A3 + ... + An
;   k=1
; 如果n无穷大，可以记作：
;        n
; lim   Σ Ak = A1 + A2 + A3 + ...
; n->∞  k=1
; (其中，n = 0时，该和式值为0，
;  k <= n，起始值为1，逐步累加1直到等于n
;  公式中k, 1 ... n为下标，不代表相乘)
; 
;  3
; Σ 2k + 1 = (2 * 1 + 1) + (2 * 2 + 1) + (2 * 3 + 1)
;  k=1

;;;; 算法设计
; 上述插入算法采用了增量法：
; 在排序子数组A[1 ... j-1]后，添加当个元素到适当位置，产生子数组A[1 ... j]
; 
; 另一种高效的方法是｀分治法｀，它的优点之一是很容易确定运行时间
