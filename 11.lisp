(defpackage #:alg-11
 (:use :cl))

(in-package #:alg-11)

;;;; 11 散列表

; 在最坏的情况下，散列表hash table查找一个元素的时间与链表相同，达到O(n)
; 然而在实际应用中它的性能是极好的，在一个合理的假设下，在散列表中查找一个元素的平均时间是O(1)

;;;; 11.1 直接寻址表(direct-address table)

; 当关键字的全域U比较小时，直接寻址是一种简单有效的技术
; 我们用一个数组表示直接寻址表T[0...n]，它的操作实现如下：
; DIRECT-ADDRESS-SEARCH(T, k)
; 	return T[k]
; 
; DIRECT-ADDRESS-INSERT(T, X)
; 	T[x.key] = x
; 
; DIRECT-ADDRESS-DELETE(T, x)
; 	T[x.key] = nil
; 
; 上述每个操作，只需要O(1)的时间
; 直接寻址技术缺点也很明显，如果全域U很大，这需要很大的容量
