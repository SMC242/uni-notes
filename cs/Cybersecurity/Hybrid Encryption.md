# Overview
[[Public-key Encryption]] (PKE) has some notable downside when compared to [[Symmetric Cryptography|symmetric key encryption]] (SKE)

- The sender and receiver are not interchangeable
	- Only the party with the private key can decrypt messages
- PKE is slow
- SKE can encrypt arbitrary length messages, PKE can't

The solution: combine them

# Protocol
1. Party 1 generates a private key $sk$ and a public key $pk$
2. Party 2 receives the private key somehow
3. Party 2 generates a symmetric encryption key $k \leftarrow \{0, 1\}^k$
4. Party 2 sends:
	1. The symmetric key encrypted with the public key $C_{k} \leftarrow Enc_{pk}(k)$
	2. The message encrypted with the symmetric key $C_{M} = Enc_{k}(M)$
5. Party 1 decrypts the symmetric key $k \leftarrow Dec_{sk}(C_{k})$
6. Party 1 now knows $k$ and can decrypt the message $M \leftarrow Dec_{k}(C_{M})$

