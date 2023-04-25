---
tags: ADT/Queue
aliases: Dequeue
---

# Definition
A type of [[Queue]] where you can insert and delete at both ends. Also known as a Dequeue

See also: [[Queue]]

# Motivation
## Use case
- Browser history
	- Recently-visited URLs at the front
	- Old URLs are popped off after a certain number of `enqueue`s

# Comparison
Compared to [[Bounded Queue]]:
<ul class="breakdown">
	<li class="pro">Elements can be added from the front and back of the queue</li>
</ul>

# Members
![[Bounded Queue#Members]]

# Operations
## Push-back
- Add an element to the end of the queue ([[Bounded Queue#Enqueue|enqueue]])
- [[Time Complexity]]: $O(1)$

## Push-front
- Add an element to the front of the queue ("unshift")
- [[Time Complexity]]: $O(1)$

## Pop-back
- Remove and return the last element
- [[Time Complexity]]: $O(1)$

## Pop-front
- Remove and return the first element ([[Bounded Queue#Dequeue|dequeue]] or "shift")
- [[Time Complexity]]: $O(1)$
