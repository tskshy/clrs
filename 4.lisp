(defpackage #:alg-4
  (:use :cl))

(in-package #:alg-4)
;;;; 4 分治策略

;;;; 4.1 最大子数组问题
(defun find-maximum-subarray-1 (arr)
  "暴力穷举"
  (let ((len (length arr))
	(start 0)
	(end 0)
	(max-sum (aref arr 0)))
    (dotimes (i len (values start end max-sum))
      (let ((tmp 0))
	(do ((j i (1+ j)))       
	    ((>= j len))
	  (setf tmp (+ tmp (aref arr j)))
	  (if (> tmp max-sum)
	      (progn
		(setf max-sum tmp)
		(setf start i)
		(setf end j))))))))

; 设`do`平均运行时间代价为t
; 则一次dotimes时间代价为 n * t / 2 (大概的运行次数)
; 所以暴力穷举的时间复杂度：n * (n * t / 2) ==> O(n^2)

(defun find-max-crossing-subarray (arr low mid high)
  "
寻找跨越下标mid的最大子数组
FIND-MAX-CROSSING-SUBARRAY(A, low, mid, higt)
    left-sum = -∞
    sum = 0
    for i = mid downto low
        sum = sum + A[i]
        if sum > left-sum
            left-sum = sum
            max-left = i

    right-sum = -∞
    sum = 0
    for j = mid + 1 to high
        sum = sum + A[j]
        if sum > right-sum
            right-sum = sum
            max-right = j

    return (max-left, max-right, left-sum + right-sum)
"
  (let ((left-sum (aref arr mid))
	(right-sum (aref arr (+ mid 1)))
	(max-left mid)
	(max-right (+ mid 1))
	(sum1 0)
	(sum2 0))
    (do ((i mid (1- i)))
	((< i low))
      (progn
	(setf sum1 (+ sum1 (aref arr i)))
	(if (> sum1 left-sum)
	    (progn
	      (setf left-sum sum1)
	      (setf max-left i)))))
    (do ((j (+ mid 1) (1+ j)))
	((> j high))
      (progn
	(setf sum2 (+ sum2 (aref arr j)))
	(if (> sum2 right-sum)
	    (progn
	      (setf right-sum sum2)
	      (setf max-right j)))))
    (values max-left max-right (+ left-sum right-sum))))

(defun find-maximum-subarray (arr low high)
  "
寻找最大子数组 分治算法
FIND-MAXIMUM-SUBARRAY(A, low, high)
    if low == high
        return (low, high, A[low]) //only one element

    else
        mid = floor((low+high)/2)
        (left-low, left-high, left-sum) = FIND-MAXIMUM-SUBARRAY(A, low, mid)
        (right-low, right-high, right-sum) = FIND-MAXIMUM-SUBARRAY(A, mid + 1, high)
        (cross-low, cross-high, cross-sum) = FIND-MAX-CROSSING-SUBARRAY(A, low, mid, high)

        if left-sum >= right-sum and left-sum >= cross-sum
            return (left-low, left-high, left-sum)
        elseif right-sum >= left-sum and right-sum >= cross-sum
            return (right-low, right-high, right-sum)
        else
            return (cross-low, cross-high, cross-sum)
"
  (if (= low high)
      (values low high (aref arr low))
      (let ((mid (floor (/ (+ low high) 2))))
	(multiple-value-bind (left-low left-high left-sum) (find-maximum-subarray arr low mid)
	  (multiple-value-bind (right-low right-high right-sum) (find-maximum-subarray arr (+ mid 1) high)
	    (multiple-value-bind (cross-low cross-high cross-sum) (find-max-crossing-subarray arr low mid high)
	      (cond ((and (> left-sum right-sum) (> left-sum cross-sum)) (values left-low left-high left-sum))
		    ((and (> right-sum left-sum) (> right-sum cross-sum)) (values right-low right-high right-sum))
		    (t (values cross-low cross-high cross-sum)))))))))


; 将分治法理解成一个树形结构
; 每层运行代价为cn
; 如果规模为n(即叶子数为n)
; 根据二叉数性质 2^x = n => x = lgn 
; => 总时间 (x + 1) * cn => O(nlgn)

