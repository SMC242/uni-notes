---
tags:
  - TypeTheory
---
# Overview
Lists are a [[Types#Recursive types|recursive type]] that represents a sequence of 0 or more components

A list has two cases:
- Empty
- Non-empty: there is a head (first element) and a tail (the rest)

$$
\begin{align*}
LIST &= empty \ VOID\\
&+ nonempty (T \times LIST)
\end{align*}
$$
> [!NOTE]
> - $T$ is the head
> - $LIST$ is the tail

