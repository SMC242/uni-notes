# Linked list queue
- A queue using a [[Linked List]] internally
- `head = LinkedList.head`
- `tail = LinkedList.tail`
- [[#enqueue]] = [[Linked List#Insert-tail|insert-tail]] ($O(1)$)
- [[#enqueue]] = [[Linked List#Delete-head|delete-head]] ($O(1)$)
- [[#Queue-size|size]] would be $O(n)$ unless you store a `size` variable
- Not vulnerable to overflows
- Using a [[Linked List#Doubly-linked lists|doubly-linked list]] doesn't improve performance and worsens memory consumption