(defpackage #:alg-10
 (:use :cl))

(in-package #:alg-10)

;;;; 10 基本数据结构

;;;; 10.1 栈和队列

; 栈(Stack)：后进先出(Last In First Out, LIFO)
; 属性：top
; 基本操作：stack-empty(S)/push(S, x)/pop(S)

; 队列(queue)：先进先出(First In First Out, FIFO)
; 属性：head/tail
; 基本操作：入队enqueue(Q, x)/出队dequeue(Q)

;;;; 10.2 链表
; 链表的每个元素可以看作一个对象
; 最简单的情况下，包含至多3种属性：prev data next

; prev和next决定上一个和下一个元素
; 基于这种特性，链表操作的时间复杂度如下：
; 搜索search，O(n)
; 插入insert，O(1)
; 删除delete，O(1)

