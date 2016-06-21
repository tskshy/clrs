(defpackage #:alg-7
  (:use :cl))

(in-package #:alg-7)

;;;; 快速排序

; 快速排序的最坏时间复杂度为O(n^2)，虽然最坏时间复杂度很差
; 但在实际排序应用中是最好的选择
; 它的期望复杂度是nlgn，而且也是原址排序


;;;; 7.1 快速排序描述

; 与归并排序一样，它采用了分治思想，下面是快速排序对数组A[l ... r]三步分治过程：

; 分解：数组A[l...r]被划分成为两个(可能为空)子数组A[l...m-1]和A[m+1...r]
; 使得A[l...m-1]中的任一元素都小于等于A[m]，而A[m]也小于等于A[m+1...r]中的任一元素
; 其中，计算下标m也是划分的一部分

; 解决：通过递归调用快速排序，对子数组A[l...m-1]和A[m+1...r]进行排序

; 合并：因为子数组都是原址排序的，所以不需要合并操作：数组A[l...r]已经有序

(defun quick-sort (arr l r)
  "
QUICK-SORT(A, l, r)
    if l < r
        m = PARTITION(A, l, r)
        QUICK-SORT(A, l, m - 1)
        QUICK-SORT(A, m + 1, r)
"
  (let ((m nil))
    (if (< l r)
	(progn
	  (setf m (partition arr l r))
	  (quick-sort arr l (- m 1))
	  (quick-sort arr (+ m 1) r)))
    arr))

(defun partition (arr l r)
  "
PARTITION(A, L, R)
    k = A[R]
    i = L
    for j = L to R - 1
        if A[j] <= x
            exchange A[i] with A[j]
            i = i + 1

    exchange A[i] with A[R]
    return i

大致思路就是这样，数组A[L...I...J...R]
区间[L...I]存放比key小或者等于的元素
区间[I...J]存放比key大的元素
区间[J...R]是未比较元素
当J接近到R时，由于最终有一步是i = i + 1
所以下标i此时的元素是大于key值的，交换他们之后
数组就变成这样的情况：
以下标i为界，前面部分都是比它小或者等的，后面部分都是大于它的
"
  (let ((k (aref arr r))
	(i l)
	(dummy nil))
    (do ((j l (1+ j)))
	((>= j r))
      (if (< (aref arr j) k)
	  (progn
	    (setf dummy (aref arr i))
	    (setf (aref arr i) (aref arr j))
	    (setf (aref arr j) dummy)
	    (setf i (+ i 1)))))
    (setf dummy (aref arr i))
    (setf (aref arr i) (aref arr r))
    (setf (aref arr r) dummy)
    (values i arr)))



