---
aliases: RSA-FDH
---
# Overview
A [[Cryptographic Signing|digital signing algorithm]] built on top of [[RSA Encryption]] 

# Key generation
![[RSA Encryption#Key generation]]

# Signing
1. Take in a private key $(d,n)$ and a message $M$
2. Compute $\sigma \leftarrow H(M)^{d} \mod n$
	- $H(m)$ is a [[Hashing|hash  function]] in the range $\{0, 1, \dots , n - 1 \}$

# Verification
1. Take in a public key $(e,n)$, a message $M$, a signature $\sigma$
2. Output $\sigma^{e} \mod n \stackrel{?}{=} H(M)$
