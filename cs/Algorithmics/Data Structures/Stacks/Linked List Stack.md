---
tags: ADT/Stack

---
# Linked list stacks
- A [[stack]] using a [[Linked List]] for its internal list
- `top = LinkedList.head`
- [[#push]] = [[Linked List#Insert|insert-head]] ($O(1)$)
- [[#pop]] = [[Linked List#Delete-head|delete head]] ($O(1)$)
- [[#size]] is $O(n)$ unless you maintain a `size` variable
- Not vulnerable to stack overflowss