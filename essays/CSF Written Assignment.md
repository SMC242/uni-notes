---
tags: Essay/SubjectName

---
<small>
	2560102M
	<br/>
	Eilidh McAlonan
</small>

1)
a)
- Collision resistance is when for any $x$, $f(x)$ will output a string that is unique to the degree that it would be infeasible to find another input with the same output
- $LSB(x)$ will return final bit of a binary number (either the leftmost digit in big endian notation or the rightmost digit in little endian)
- The set of return values for $LSB(x)$ is $\{0, 1\}$
	- This means that the $LSB$ of half of all non-empty will collide
	- $LSB(x)$ is not collision-resistant
- $LSB(x)$ contributes only one bit to the output of $H'(x)$
- $H(x)$ is known to be collision-resistant, meaning that it would be infeasible to find two $x$s with a matching $H(x)$
- While $LSB(x)$ is not collision resistant, $H(x)$ is. As $LSB(x)$ only contributes a single bit to the output and that bit could be part of half of all binary strings, it does not provide enough information about the original string to reconstruct it and therefore a malicious input would be infeasible to construct from an $H(x)$
- Therefore, $H'(x)$ is collision-resistant

b)
- Assumption: the attacker has access to $H(x)$
- An attacker with access to the password file would be able to search the second element of the triple $T_2$ for known hashes
- If a known hash $H(P*)$ is found, the attacker will know the corresponding password $P*$. Additionally, the attacker will know the user ID $U$ from the triple
- Given $P*, H(P*)$, the third element of the triple $T_3$, $P$ could be reconstructed by computing $H(P*) || P*$ and applying $H(x)$ to the result
- If the attacker has access to $H(x)$, they would be able to compute $T_3$ using that process
- Now that the attacker has $(U, P*, H(H(P*) || P*))$, they would be able to access the account identified by $U$
- If the hacker does not have access to $H(x)$, they would not be able to breach this security, however this system will be vulnerable as soon as $H(x)$ has been reverse-engineered

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

![[role-hierarchy-v2.png]]
- Senior, External, and Junior are mutually exclusive
- A user with the Junior role must have a specific section role (Mathematics Junior, Physics Junior, or Chemistry Junior)
- Junior provides the base permissions for juniors, while section-specific junior roles provide access to a section

| Role               | Mathematics section | Physics section | Chemistry section | New books | Old books |
| ------------------ | ------------------- | --------------- | ----------------- | --------- | --------- |
| Senior             | Yes                 | Yes             | Yes               | Yes       | Yes       |
| External           | Yes                 | Yes             | Yes               | No        | Yes       |
| Junior             | No                  | No              | No                | Yes       | Yes       |
| Mathematics Junior | Yes                 | No              | No                | Yes       | Yes       |
| Physics Junior     | No                  | Yes             | No                | Yes       | Yes       |
| Chemistry          | No                  | No              | Yes               | Yes       | Yes       |

ii)

- $Section(b)$ is the section of a book $b$
	- Domain: $\{ mathematics, physics, chemistry \}$
- $Position(u)$ is the position of the user $u$ in the library
	- Domain: $\{senior, external, mathematics\ junior, physics\ junior, chemistry\ junior\}$
- $Age(b)$ is the age of a book $b$
	- Domain: $\{old, new\}$

Rules (access is granted when the rule evaluates to $true$):
- $Position(u) = external \land Section(b) \in \{ mathematics, physics, chemistry\}  \land  Age(b) = old$
- $Position(u) = senior \land Section(b) \in \{ mathematics, physics, chemistry\} \land Age(b) \in \{old, new\}$
- $Position(u) = mathematics\ junior \land Section(b) = mathematics \land Age(b) \in \{old, new\}$
- $Position(u) = physics\ junior \land Section(b) = physics \land Age(b) \in \{old, new\}$
- $Position(u) = chemistry\ junior \land Section(b) = mathematics \land Age(b) \in \{old, new\}$

3)
a)

| x   | P1     | P2     | P3     | P4     |
| --- | ------ | ------ | ------ | ------ |
| P1  | P1, P1 | P1, P2 | P1, P3 | P1, P4 |
| P2  | P2, P1 | P2, P2 | P2, P3 | P2, P4 |
| P3  | P3, P1 | P3, P2 | P3, P3 | P3, P4 |
| P4  | P4, P1 | P4, P2 | P4, P3 | P4, P4 |

b)

- Assumption: both sensors generate a signal
- Using conditional probability: $P(A | B) = \frac{P(A \cap B)}{P(B)}$
- $P(medium | P3) = \frac{P(P2 + P3)}{P3}$
- There are 16 possible pairs
- There are 7 pairs including a P3
	- $P(P3) = \frac{7}{16}$
- The pairsP3, P2 is required to generate a threat level of "medium"
- There are 2 pairs including a P2 and a P3
	- $P(P2 \cap P3) = \frac{2}{16}$
- Probability of "medium" threat level = $\frac{\frac{2}{16}}{\frac{7}{16}} = \frac{2}{7}$

c)
- Assumption: both sensors generate a signal
- Using conditional probability: $P(A | B) = \frac{P(A \cap B)}{P(B)}$
- $P(severe | P4) = \frac{P(P4 + P4)}{P4}$
- There are 16 possible pairs
- There are 7 pairs including a P4
	- $P(P4) = \frac{7}{16}$
- The pair P4, P4 is required to generate a threat level of "medium"3
- There is only one pair including P4, P4
	- $P(P4 \cap P4) = \frac{1}{16}$
- Probability of "medium" threat level = $\frac{\frac{1}{16}}{\frac{7}{16}} = \frac{1}{7}$

4)
a)

Setup phase:
1. Each bidder generates their public-private key pair $sk_{b_m}, pk_{b_m} \leftarrow PKE.Gen()$
2. Each bidder sends a message to $CA$ containing their ID $ID_{b_{m}}$ and public key $pk_{b_m}$
3. CA grants certificates to each bidder $cert_{b_m} \leftarrow Sign_{sk_{b_m}}(ID_{b_m} || pk_{b_m})$
4. Each bidder sends their certificate, ID, and public key to AA and AA verifies the certificates $Vrfy_{pk_{b_m}}(ID_{b_m} || pk_{b_m}, cert_{b_m}) \stackrel{?}{=} 1$
	1. Invalid certificates are rejected
5. AA must do the same, except it will send its certificate, ID, and public key to the bidders (achieves mutual authentication)
6. AA will send a switch cipher message to the bidders. All communication from this point will be encrypted

Bidding phase:
1. Each bidder sends a message containing the item ID $I$ and their bid $B$. These messages are encrypted under AA's public key $C_{b_m} \leftarrow Enc_{pk_{AA}}(I||B)$. These messages are also signed with the bidder's digital signature $S_{b_m} \leftarrow Sign_{pk_{b_{m}}}(I || B)$
2. AA verifies the signatures of each message and decrypts them if verified successfully $Vrfy_{pk_{b_m}}(C_{b_{m}, S_{b_m}}) \stackrel{?}{=} 1$ then $M_{b_{m}}= Dec_{sk_AA}(C)$
3. The bids are compared

Result phase:
1. AA sends a message containing the winning ID to all bidders. This message is encrypted under each bidder's public key. $C_{b_m} \leftarrow Enc_{pk_{b_m}}(ID)$
2. Each bidder decrypts the message with their private key $M_{AA} = Dec_{sk_{b_m}}(C_{b_m})$

b)
1. Bidders can't decrypt each others' bids because only AA knows the private key used when decrypting bids
2. If another bid from the same ID is received, it can be rejected
3. AA knows that messages are not forged because they are digitally signed
4. My implementation does not meet this requirement. AA's messages could be signed to fix this