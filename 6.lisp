(defpackage #:alg-6
  (:use :cl))

(in-package #:alg-6)


;;;; 6 堆排序

;;;; 6.1 堆

; 如果输入数组中仅有常数个元素需要在排序过程中存储在数组外
; 则称排序算法是原址的(in place)

; 插入排序是一种原址排序
; 而归并排序中， merge过程并不是原址的

; 堆排序是原址的，时间复杂度为O(nlgn)，它具有插入排序和归并排序的优点

(defun parent (i)
  "根据节点下标求父节点 i [0 ...]
"
  (if (= i 0)
      0
      (floor (/ (- i 1) 2))))

(defun left (i)
  "i [0 ...]"
  (+ (* 2 i) 1))

(defun right (i)
  "i [0 ...]"
  (* 2 (+ i 1)))

; 二叉堆的两种形势：
; 最大堆：除了根以外的所有节点i都满足 A[parent(i)] >= A[i]
; 最小堆：除了根以外的所有节点i都满足 A[parent(i)] <= A[i]

; 堆排序算法中，一般使用最大堆，最小堆通常用于有限队列

; 如果把堆看成一棵树，如果包含n个元素的堆可以看成一棵完全二叉树
; 那么该堆的高度就是lgn，堆结构上的一些基本操作的运行时间至多与树的高度成正比
; 即时间复杂度为O(lgn)


;;;; 6.2 维护堆的性质

(defun max-heapify (arr i &optional (root-index 0))
  "
用于维护最大堆性质的重要过程
arr - 数组
i - 下标(相对数组, 以起始下标为准的新数组)
root-index - 起始下标(整个数组)

输入数组arr和下标i
调用该函数时，假设父节点left(i)和right(i)的二叉树都是最大堆
但A[i]可能小于其孩子，这就违背了最大堆的性质
MAX-HEAPIFY通过让A[i]的值在最大堆中逐渐降级
从而使得以下标i为根节点的子树重新遵守最大堆的性质

MAX-HEAPIFY (A, i, root-index)
    l = left(i) + root-index
    r = right(i) + root-index
    heap-size = A.heap-size - root-index
    new-i = i + root-index

    if l < heap-size - root-index and A[l] > A[new-i]
        largest = l
    else
        largest = new-i

    if r < heap-size - root-index and A[r] > A[largest]
        largest = r

    if largest != new-i
        exchange A[new-i] with A[largest]
        MAX-HEAPIFY (A, largest, root-index)

对于一个以i为根节点，大小为n的子树，MAX-HEAPIFY的时间代价包括：
调整A[i]/A[left(i)]/A[right(i)]的关系的时间代价O(1)
在加上一棵以i的一个孩子节点为根节点的子树运行MAX-HEAPIFY的时间代价(假设递归调用会发生)
因为每个孩子子树的大小至多为2n/3(最坏情况发生在树的最底层恰好半满的时候)
T(n) <= T(2n/3) + O(1) ==> T(n) = O(lgn)
"
  (let ((heap-size (- (length arr) root-index))
	(l (+ (left i) root-index))
	(r (+ (right i) root-index))
	(new-i (+ i root-index))
	(largest (+ i root-index))
	(dummy nil))
    (if (and (< (- l root-index) heap-size) (> (aref arr l) (aref arr new-i)))
	(setf largest l))
    (if (and (< (- r root-index) heap-size) (> (aref arr r) (aref arr largest)))
	(setf largest r))
    (if (/= largest new-i)
	(progn
	  (setf dummy (aref arr new-i))
	  (setf (aref arr new-i) (aref arr largest))
	  (setf (aref arr largest) dummy)
	  (max-heapify arr largest root-index))
	arr)))

#+test
(let ((arr (vector 16 4 10 14 7 9 3 2 8 1)))
  ; arr result vector(16 14 10 8 7 9 3 2 4 1)
  (max-heapify arr 1))

#+test
(let ((arr (vector 16 4 10 14 7 9 3 2 8 1)))
  ; arr result vector(16 4 10 14 7 9 3 8 2 1)
  (max-heapify arr 0 7))

;;;; 6.3 建堆
(defun build-max-heap (arr root-index)
  ;i为相对数组里的下标，取值范围[0 ... ]
  (do ((i (- (length arr) 1) (1- i)))
      ((< i 0) arr)
    (max-heapify arr i root-index)))

#+test
(let ((arr (vector 4 1 3 2 16 9 10 14 8 7)))
  ; result: (vector 16 14 10 8 7 9 3 2 4 1)
  (build-max-heap arr))

#+test
(let ((arr (vector 4 1 3 2 16 9 10 14 8 7)))
  ; result: (vector 4 1 3 2 16 9 14 10 8 7)
  (build-max-heap arr 5))

; 每次调用 max-heapify的时间复杂度是lgn
; build-max-heap需要O(n)次这样的调用
; 因此总的时间复杂度就是O(nlgn)
; 这个上界虽然正确 但不是渐进紧确的
; 但是根据如下性质可以得到一个更紧确的界：
; 包含n个元素的堆的高度为floor(lgn)，高度为h的堆最多包含celing(n/2^(h+1))个节点
; 最终可以推导出 build-max-heap为O(n)
; 目前不太清楚 build-max-heap => O(n) --!

;;;; 6.4 堆排序算法

(defun heap-sort (arr)
  "
build-max-heap初始化整体数组为最大堆
在相对数组里，首元素为最大，每次维护首元素下标后的元素的最大堆性质

HEAP-SORT(A)
    BUILD-MAX-HEAP(A, 0)
    for i = 0 until A.length
        MAX-HEAPIFY(A, 0, i)
"
  ;(do ((i 0 (1+ i)))
  ;    ((>= i (length arr)) arr)
  ;  (build-max-heap arr i)) ; 时间复杂度 O(n^2)，弃用
  (build-max-heap arr 0) ;O(n) or O(nlgn)，但最终对heap-sort的时间复杂度无影响
  (do ((i 1 (1+ i)))
      ((>= i (length arr)) arr)
    (max-heapify arr 0 i)) ;O(nlgn)
  )

#+test
(let ((arr (vector 5 4 1 3 1 2 8 2 1 1 2 3 4 5)))
  (heap-sort arr))

#+test
(let ((arr (vector 1 2 3 4 5)))
  (max-heapify arr 4 0)
  (max-heapify arr 3 0)
  (max-heapify arr 2 0)
  (max-heapify arr 1 0)
  (max-heapify arr 0 0) ; 5 4 3 1 2
  
  (max-heapify arr 3 1)
  (max-heapify arr 2 1)
  (max-heapify arr 1 1)
  (max-heapify arr 0 1) ; 5 4 3 1 2
  
  (max-heapify arr 2 2)
  (max-heapify arr 1 2)
  (max-heapify arr 0 2) ; 5 4 3 1 2
  
  (max-heapify arr 1 3)
  (max-heapify arr 0 3) ; 5 4 3 2 1

  (max-heapify arr 0 4) ; 5 4 3 2 1
  )


;;;; 6.5 优先队列
; 优先队列(priority queue)是一种用来维护由一组元素构成的集合S的数据结构
; 其中每一个元素都有一个相关的值，称为关键字(key)
; 一个最大优先队列支持以下操作：
; INSERT(S, x)：插入元素x到集合S中
; MAXIMUM(S)：返回S中具有最大关键字的元素
; EXTRACT-MAX(S)：去掉并返回S中具有最大关键字的元素
; INCREASE-KEY(S, x, k)：将元素x的关键字值增加到k，这里假设k不小于x原关键字值
; 
; 相应的，最小优先队列支持的操作为：
; INSERT(S, x)
; MINIMUM(S)
; EXTRACT-MIN(S)
; DECREASE-KEY(S, x, k)
; 
; 显然，优先队列可以用堆实现 ... TODO

