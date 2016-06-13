(defpackage #:alg-2
  (:use :cl))

(in-package #:alg-2)

; ### 笔记
;;;; 2 算法基础

;;;; 2.1 插入排序
;;;; 实现升序或降序排列
(defun insertion-sort (arr symbol)
  "
arr is of type LIST
symbol must be `>` or `<`
                                      执行时间          执行次数
for j = 1 until arr.length            c1               n
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

;;;; 2.2 算法分析

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
;  n
;  Σ Ak = A1 + A2 + A3 + ... + An
;  k=1
; 如果n无穷大，可以记作：
;       n
; lim   Σ Ak = A1 + A2 + A3 + ...
; n->∞ k=1
; (其中，n = 0时，该和式值为0，
;  k <= n，起始值为1，逐步累加1直到等于n
;  公式中k, 1 ... n为下标，不代表相乘)
; 
; 3
; Σ 2k + 1 = (2 * 1 + 1) + (2 * 2 + 1) + (2 * 3 + 1)
; k=1

;;;; 2.3 算法设计
; 上述插入算法采用了增量法：
; 在排序子数组A[1 ... j-1]后，添加当个元素到适当位置，产生子数组A[1 ... j]
; 
; 另一种高效的方法是｀分治法｀，它的优点之一是很容易确定运行时间

; 分治法思想：
; 将原问题分解为几个规模较小但类似于原问题的子问题
; 递归求解这些子问题
; 最后合并这些子问题的解来简历原问题的解

; 并归排序算法完全遵守分治模式，即
; 分解：分解待排序的n个元素的序列成各具n/2个元素的两个子序列
; 解决：使用并归排序递归地排序两个子序列
; 合并：合并两个已排序的子序列以产生已排序的答案

; 而它的关键操作就是“合并”：两个'已排序'序列的合并
; 下面，可以定义一个辅助函数merge(arr, l, m , r)来完成合并

(defun ~merge (arr l m r)
  "
`包CL已经含有merge名称的函数 此处需要换一个名称`
arr(ay) - 数组
l - left 下标
m - mid 下标
r - right 下标

MERGE(array, l, m, r)
    n1 = m - l + 1
    n2 = r - m
    let al[n1] and ar[n2] be new arrays

    for i = 0 until n1
        al[i] = array[l + i]

    for j = 0 until n2
        ar[j] = array[m + j + 1]

    i = 0, j = 0

    for k = l to r
        case i < n1 and j < n2
            if al[i] <= ar[j]
                array[k] = al[i]
                i =  i + 1
            else
                array[k] = ar[j]
                j =  j + 1
        case i < n1
            array[k] = al[i]
            i = i + 1
        case j < n2
            array[k] = ar[j]
            j = j + 1
"
  (let* ((n1 (+ 1 (- m l)))
	 (n2 (- r m))
	 (n3 (+ n1 n2))
	 (al (make-array `(,n1) :initial-element nil))
	 (ar (make-array `(,n2) :initial-element nil)))
    (dotimes (i n1)
      (setf (svref al i) (aref arr (+ i l))))
    (dotimes (j n2)
      (setf (svref ar j) (aref arr (+ j m 1))))
    (let ((i 0)
	  (j 0))
      (do ((k l (1+ k)))
	  ((>= k (+ n3 l)) arr)
	(cond ((and (< i n1) (< j n2))
	       (if (<= (aref al i) (aref ar j))
		   (progn
		     (setf (aref arr k) (aref al i))
		     (setf i (+ i 1)))
		   (progn
		     (setf (aref arr k) (aref ar j))
		     (setf j (+ j 1)))))
	      ((< i n1)
	       (progn
		 (setf (aref arr k) (aref al i))
		 (setf i (+ i 1))))
	      ((< j n2)
	       (progn
		 (setf (aref arr k) (aref ar j))
		 (setf j (+ j 1)))))))))

; 在~merge函数中，3个for循环运行时间都跟数组长度相关
; 其余步骤可用常量c代替
; 所以次函数时间复杂度可以简化成
; c1 * n1 + c2 * n2 + c3 * n3 + c ==> 线性阶 ==> O(n)


(defun merge-sort (arr l r)
  "
归并排序数组A[l ... r]
如果l >= r，则表示子数组最多只有一个元素，可以当作已排列数组序列

MERGE-SORT(arr, l, r)
    if l < r
        m = |(l + r) / 2|
        MERGE-SORT(arr, l, m)
        MERGE-SORT(arr, (m + 1), r)
        MERGE(arr, l, m, r)
"
  (if (< l r)
      (let ((m (floor (+ l r) 2)))
	(merge-sort arr l m)
	(merge-sort arr (+ m 1) r)
	(~merge arr l m r))))

;;;; 2.3.2 分析并归排序算法

; 假设并归排序n个数的最坏情况的运行时间为T(n)，按照分治思想模型：
; 分解：分解步骤仅仅计算子数组的中间位置，需要常量时间，设为D(n)
; 解决：递归求解两个规模均为n/2的子问题，将运行2T(n/2)的运行时间
; 合并：合并函数的时间复杂度为线性阶


; 所以，时间复杂度由2T(n/2)决定

; 为方便起见，假设n刚好是2的幂，lgN代表2为底N的对数
; 将T(n)分解为树形结构，设根(第0层)运行代价为cn，则第一层代价就为 cn/2 + cn/2 ...

; -      层                              项                              代价
; |      0                               cn                              cn
; |                                    /    \
; |      1                          cn/2   cn/2                          cn
; lgn层                              /   \   /   \      
; |      2                       cn/4 cn/4 cn/4 cn/4                     cn
; |                             /  \  /  \ /  \ /  \
; _      ...                   c...      ...     ...c                    cn
; 
; 所以总代价为 (lgn + 1) * cn + D => O(nlgn)

; 后记，插入排序时间复杂度为O(n^2)，并归排序为O(nlgn)，
; 所以，设置适当的分解因子，最后层数的子问题采用插入排序，效率更高
