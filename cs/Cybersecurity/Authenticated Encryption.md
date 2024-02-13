# Overview
Combining integrity verification methods with [[Cryptography]]

# Encrypt-then-authenticate
- Encrypt the message $C \leftarrow Enc_{k1}(M)$
- Then generate the [[Message Integrity#Message authentication codes|MAC]] for the [[Cryptography#Key definitions|ciphertext]] $t \leftarrow Mac_{k_{2}}(C)$
- The other part verifies the MAC and decrypts it
	1. Check if $Vrfy_{k_{2}}(C,t) = 1$
	2. If so, decrpyt  $M \leftarrow Dec_{k_{1}}(C)$

There are more efficient methods than this such as :
- Galois Counter Mode (GCM)
- Offset Codebook Mode (OCB)
