---
tags: ADT/Queue 
---

# Overview
A [[Queue]] using a [[Linked List]] internally

See also:
- [[Queue]]
- [[Linked List]]

# Implementation
- `head = LinkedList.head`
- `tail = LinkedList.tail`
- [[#enqueue]] = [[Linked List#Insert-tail|insert-tail]] ($O(1)$)
- [[#enqueue]] = [[Linked List#Delete-head|delete-head]] ($O(1)$)
- [[#Queue-size|size]] would be $O(n)$ unless you store a `size` variable
- Not vulnerable to overflows
- Using a [[Linked List#Doubly-linked lists|doubly-linked list]] doesn't improve performance and worsens memory consumption