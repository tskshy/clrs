(defpackage #:alg-8
  (:use :cl))

(in-package #:alg-8)

;;;; 8 线性时间排序

; 在排序的最终结果中，各元素之间的次序依赖于它们的比较关系，这样的排序算法称为`比较排序`
; 在之前笔记中涉及到的算法，包括`插入，归并，快速，堆`排序，都属于比较排序

; 后面介绍的三种线性时间复杂度的排序算法：计数排序，基数排序，桶排序
; 这些运用的是运算而不是比较来确定排序顺序的

;;;; 8.2 计数排序

; 计数排序基本思想：对于每一个输入元素，确定小于x的元素的个数
; 利用这一信息，可以直接把元素放在数组对应的下标中

(defun counting-sort (arr)
  "
COUNTING-SORT(A, B, k)
    //数组B存放排序的输出
    let C[0...k] be a new array //提供临时存储空间
    for i = 0 until k
        C[i] = 0                //初始化C的元素

    for j = 0 until A.length
        C[A[j]] = C[A[j]] + 1   //C[i]代表A[n]元素个数(A[n] == i，i = 0, 1 ... k)

    for i = 0 until k
        C[i+1] = C[i+1] + C[i]  //对每一个i=0,1...k，统计多少输入元素是小于或等于i的

    for j = A.length - 1 downto 0
        B[C[A[j]] - 1] = A[j]
        C[A[j]] = C[A[j]] - 1   //把每个元素A[j]放到它在输出数值B中的正确位置
                                //如果所有元素互异，对于每一个A[j]值来说
                                //C[A[j]]就是A[j]在输出数值中的最终正确位置
                                //这是因为有C[A[j]]个元素小于或等于A[j]
                                //但有可能所有元素不是互异的
                                //所以将每一个值A[j]放入B中后，都要将C[A[j]]值减一
                                //这样，如果遇到下一个值等于A[j]的元素时
                                //可以直接放在输出数组B中A[j]的前一个位置

计数排序的一个重要性质是它是稳定的：具有相同值的元素在输出数组中的相对次序与它们在输入数组中的相对次序相同
代码实现上用到了比较性质，用于确定k值
而且由于编程语言的特性
加上对偏移的计算
实现上跟伪代码略微不同

显然，每重循环都是线性时间，最终时间复杂度表示为O(n)
但不是原址排序，空间换时间

如果计算出来的k值过大，也就是说存在过大的输入元素，就需要对空间和时间进行考虑了
"
  (labels ((maximum (arr max i)
	     (if (< i 0)
		 max
		 (maximum arr
			  (if (or (null max) (> (aref arr i) max))
			      (aref arr i)
			      max)
			  (1- i))))
	   (minimum (arr min i)
	     (if (< i 0)
		 min
		 (minimum arr
			  (if (or (null min) (< (aref arr i) min))
			      (aref arr i)
			      min)
			  (1- i))))
	   (fn-offset (num)
	     (if (< num 0)
		 (abs num)
		 (- 0 num))))
    (let* ((len (length arr))
	   (offset (fn-offset (minimum arr nil (1- len))))
	   (k (+ 1 (+ (maximum arr nil (1- len)) offset)))
	   (arr-tmp (make-array `(,k) :initial-element 0))
	   (arr-out (make-array len)))
      (dotimes (j len)
	(setf (aref arr-tmp (+ offset (aref arr j))) (+ (aref arr-tmp (+ offset (aref arr j))) 1)))
      (dotimes (i (1- k))
	(setf (aref arr-tmp (+ i 1)) (+ (aref arr-tmp (+ i 1)) (aref arr-tmp i))))
      (do ((j (1- len) (- j 1)))
	  ((< j 0) arr-out)
	(setf (aref arr-out (1- (aref arr-tmp (+ offset (aref arr j))))) (aref arr j))
	(setf (aref arr-tmp (+ offset (aref arr j))) (- (aref arr-tmp (+ offset (aref arr j))) 1))))))



;;;; 8.3 基数排序

; 基数排序是按位分解，按照由低到高(或由高到低)有效位进行排序
; 如果有一组三位数的数字，就按照 个位－十位－百位顺序进行一位数排序
; 为了保证基数排序的正确性，一位数排序算法必须是稳定排序
; 所以，基数排序依赖一种稳定的排序算法
; 通常，一位数不太大时(泛指区间 一位数的区间也就[0-9])
; 计数排序是一个很好的选择

(defun radix-sort (arr d)
  "
RADIX-SORT(A, d)
    for i = 1 to d
        use a stable sort to sort array A on digit i
"
  "TODO")

;;;; 8.4 桶排序

; 桶排序(bucket sort)假设输入数据服从均匀分布，平均情况下它的时间代价位O(n)

; 备注：'(1 2 3)为不可变的list (list 1 2 3)可以变更list元素

(defun bucket-sort (arr)
  "
BUCKET-SORT(A)
    n = A.length
    let B[0...n-1] be a new array
    for i = 0 to n-1
        B[i] = empty list
    for i = 0 to n-1
        insert A[i] into list B[nA[i]] //把A元素插入B中对应桶区间
    for i = 0 to n-1
        sort list B[i] with insertion sort
    concatenate the lists B[0] ... B[n-1] together in order

桶排序最坏情况运行时间是O(n^2)，期望运行时间为O(n)线性阶
"
  "TODO")

