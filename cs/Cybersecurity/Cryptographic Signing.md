---
aliases:
  - Digital Signature
---
# Overview
Provide integrity by signing messages with a [[Public-key Encryption|private key]]

- Allows receivers to verify that the message was not tampered with by using the [[Public-key Encryption|public key]]
- Doesn't provide confidentiality
	- The message still has to be [[Cryptography|encrypted]]

>[!TIP] Bonus!
>Provides non-repudiation: the signer can't deny that they signed the message

# Definition
A digital signature scheme has 3 components:
- A key generator $(sk, pk) \leftarrow Gen()$
- A signing algorithm $\sigma \leftarrow Sign_{sk}(M)$
	- $\sigma$ is the signature
- A verification algorithm $b := Vrfy_{pk}(M, \sigma)$

- The following must hold: $Vrfy_{pk}(M, Sign_{sk}(M)) = 1$
- Existential unforgeability: an attacker that knows $pk$ can't generate a valid signature for a new message

# Signatures
- Attach an encrypted summary of the data before transmitting it
	- [[Hashing|Hash]] it
	- Encrypt it with the [[Cryptography#Asymmetric|private key]]
- This can then be checked on the other side
	- Hashing the received data, then comparing it with the received signature
	- The received signature can be decrypted with the [[Cryptography#Asymmetric|public key]]

# Algorithms
- [[RSA Full Domain Hash]]