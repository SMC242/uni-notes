---
tags:
  - Protocol
aliases:
  - PKE
---
# Overview
A key exchange protocol for [[Asymmetric Cryptography]] using a pair of keys

- Each party has a public and private key
- The public key is safe to share over public channels
- The private key cannot be shared

See also:
- [[Public-key Infrastructure]]

# Definition
- A public key encryption schema consists of 3 components:
	- A key generator $(sk, pk) \leftarrow Gen$
	- An encryption algorithm $C \leftarrow Enc_{pk}(M)$
	- A decryption algorithm $M \leftarrow Dec_{sk}(M)$
- $M_{1} = M_{2}$ must hold for $M_{2} \leftarrow Dec_{sk}(Enc_{pk}(M_{1}))$ 
	- I.E $Dec_{sk}$ must decrypt $Enc_{pk}$
- Acheives [[IND-CPA]]

# Protocol
1. Party 1 generates private key $sk$ and public key $pk$
2. Party 2 receives the private key somehow
	- An [[Authentication|authenticated channel]] or otherwise trusted source
3. Party 2 encrypts their message with the public key
	1. $C \leftarrow Enc_{pk}(M)$
4. Party 1 decrypts the ciphertext using the private key
	1. $M \leftarrow Dec_{sk}(C)$

