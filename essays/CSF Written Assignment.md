---
tags: Essay/SubjectName

---
<small>
	2560102M
	<br/>
	Eilidh McAlonan
</small>

2)
a) Permissions in UNIX are represented in a 4-bit format where the bits are laid out as follows (left to right):
- Sticky bit
- Owner permissions
- Group permissions
- Permissions for other users

- Permission 7 allows read, write, and execute
- All permission bits are set to 7
- Therefore, others (bit 4) are allowed to do anything with file *F*
- The sticky bit, when applied to a directory, allows only the owner to delete or execute files inside it
- While Darth has full permissions for *F*, the sticky bit will prevent him from deleting the file and harming Bob
b)
i)

3)
a)

| x  | P1     | P2     | P3     | P4     |
|----|--------|--------|--------|--------|
| P1 | P1, P1 | P1, P2 | P1, P3 | P1, P4 |
| P2 | P2, P1 | P2, P2 | P2, P3 | P2, P4 |
| P3 | P3, P1 | P3, P2 | P3, P3 | P3, P4 |
| P4 | P4, P1 | P4, P2 | P4, P3 | P4, P4 |

b)

- Assumption: both sensors generate a signal
- We know that P3 has already been generated, so the set of pairs includes only pairs with a P3
- Using conditional probability: P(A | B) = P(A and B) / P(B)
- There are two pairs of P3, P2 (ignoring order)
	- 5 pairs
- The pairs P3, P2 is required to generate a threat level of "medium"
- Probability of "medium" threat level = 1 / 5

c)
- Assumption: both sensors generate a signal
- We know a P4 has been generated already, so the set of pairs is reduced to pairs containing P4
	- 7 pairs
- The pair P4, P4 is required to generate a "severe" threat level
- There is only one combination with two P4 signals
- Probability of a severe threat = 1 / 7