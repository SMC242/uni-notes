---
aliases:
  - MAC
---
# Overview
[[Symmetric Cryptography]] methods can only defend against [[Adversaries#Passive|passive attackers]]. [[Adversaries#Active|Active attackers]] can flip bits in the ciphertext without being detected

Solution: MACs
- Provide integrity, but not confidentiality
- You can combine MACs with [[Cryptography]] in order to get both
	- See [[Authenticated Encryption]]

See first:
- [[Symmetric Cryptography]]

# Message authentication codes
AKA MACs

A triplet of algorithms:
- Key generation $k \leftarrow Gen()$
- Tag generation $t \leftarrow Mac_{k}(M)$
- Verification $b \coloneqq Vrfy_{k}(M, t)$
	- Outputs a bit $b$

> [!NOTE] Correctness
$$\forall k,M. Vrfy_{k}(M, Mac_{k}(M)) = 1$$
> I.E keys can be verified

> [!NOTE] existential unforgeability
> No attacker can generate a *valid* tag for a *new* message that wasn't sent and [[Authentication|authenticated]] by one of the legitimate parties

## CBC-MAC
A MAC mode

See [[Symmetric Cryptography#Cipher block chained mode|CBC mode]]

- Basic 
	- Not secure for variable-length messages
		- Can be remedied by using a different key $\hat{k}$
			- $\hat{t} = F_{\hat{k}}(t)$
	- Deterministic
- [[#CMAC]] and [[#HMAC]] are better for variable-length messages