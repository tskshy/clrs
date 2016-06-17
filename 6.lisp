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
  (floor (/ (- i 1) 2)))

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

(defun max-heapify (arr i)
  "
用于维护最大堆性质的重要过程
输入数组arr和下标i
调用该函数时，假设父节点left(i)和right(i)的二叉树都是最大堆
但A[i]可能小于其孩子，这就违背了最大堆的性质
MAX-HEAPIFY通过让A[i]的值在最大堆中逐渐降级
从而使得以下标i为根节点的子树重新遵守最大堆的性质

MAX-HEAPIFY (A, i)
    l = left(i)
    r = right(i)

    if l < A.heap-size and A[l] > A[i]
        largest = l
    else
        largest = i

    if r < A.heap-size and A[r] > A[largest]
        largest = r

    if largest != i
        exchange A[i] with A[largest]
        MAX-HEAPIFY (A, largest)

对于一个以i为根节点，大小为n的子树，MAX-HEAPIFY的时间代价包括：
调整A[i]/A[left(i)]/A[right(i)]的关系的时间代价O(1)
在加上一棵以i的一个孩子节点为根节点的子树运行MAX-HEAPIFY的时间代价(假设递归调用会发生)
因为每个孩子子树的大小至多为2n/3(最坏情况发生在树的最底层恰好半满的时候)
T(n) <= T(2n/3) + O(1) ==> T(n) = O(lgn)
"
  (let ((heap-size (length arr))
	(l (left i))
	(r (right i))
	(largest i)
	(dummy nil))
    (if (and (< l heap-size) (> (aref arr l) (aref arr i)))
	(setf largest l))
    (if (and (< r heap-size) (> (aref arr r) (aref arr largest)))
	(setf largest r))
    (if (/= largest i)
	(progn
	  (setf dummy (aref arr i))
	  (setf (aref arr i) (aref arr largest))
	  (setf (aref arr largest) dummy)
	  (max-heapify arr largest))
	arr)))

#+test
(let ((arr (vector 16 4 10 14 7 9 3 2 8 1)))
  ; arr result vector(16 14 10 8 7 9 3 2 4 1)
  (max-heapify arr 1))

;;;; 6.3 建堆
(defun build-max-heap (arr)
  (do ((i (- (length arr) 1) (1- i)))
      ((< i 0) arr)
    (max-heapify arr i)))

#+test
(let ((arr (vector 4 1 3 2 16 9 10 14 8 7)))
  ; result: (vector 16 14 10 8 7 9 3 2 4 1)
  (build-max-heap arr))

; 每次调用 max-heapify的时间复杂度是lgn
; build-max-heap需要O(n)次这样的调用
; 因此总的时间复杂度就是O(nlgn)
; 这个上界虽然正确 但不是渐进紧确的
; 但是根据如下性质可以得到一个更紧确的界：
; 包含n个元素的堆的高度为floor(lgn)，高度为h的堆最多包含celing(n/2^(h+1))个节点
; 最终可以推导出 build-max-heap为O(n)

;;;; 6.4 堆排序算法

(defun heap-sort (arr)
  "
初始时候，堆排序算法利用build-max-heap将输入数组A[0 ... n-1]建立成最大堆(n = A.length)
因为数组总最大元素总在根节点A[0]中，通过把它与A[n-1]互换，我们可以把该元素放到正确的位置
这时候，我们可以从堆中去掉节点n(通过减少A.heap-size实现)
新的节点组合违背了最大堆的性质，需要调用max-heapify(A, 0)
从而在A[0 ... n-2]上构建一个新的最大堆
直到从n-2降到1

HEAP-SORT(A)
    BUILD-MAX-HEAP(A)
    for i = A.length - 1 downto 1
        exchange A[0] with A[i]
        A.heap-size = A.heap-size - 1
        MAX-HEAPIFY(A, 0)
"
  (build-max-heap arr)
  (do ((i (- (length arr) 1) (1- i)))
      ((< i 1) arr)
    (let ((dummy nil))
      (setf dummy (aref arr 0))
      (setf (aref arr 0) (aref arr i))
      (setf (aref arr i) dummy)
    (max-heapify arr 0))
    ))

#+test
(let ((arr (vector 5 4 3 1 2)))
  (heap-sort arr))
