---
tags: [MathsEssay]
---

![[Set Operations#Difference]]
$$
(B \setminus A) \cup (C \setminus A)
$$
$$\forall x. (x \in ((B \setminus A) \cup (C \setminus A)) \rightarrow x \in B \setminus A \lor x \in C \setminus A)$$
$$
\forall x. (x \in ((B \setminus A) \cup (C \setminus A)) \rightarrow ((x \in B \lor x \in C) \land x \notin A))
$$
$$\forall x. (x \in B \lor x \in C) = B \cup C$$

$$\forall x. (x \in ((B \setminus A) \cup (C \setminus A)) \rightarrow ((A \cup B) \land x \notin A))$$

$$\forall x. (x \in \textrm{some set} \land x \notin A) = \textrm{some set} \setminus A$$
$$
\forall x. (x \in ((B \setminus A) \cup (C \setminus A)) \rightarrow (B \cup C) \setminus A)
$$
$$
\therefore (B \setminus A) \cup (C \setminus A) = (B \cup C) \ \setminus A
$$