---
aliases: SKE
---
# Overview
A type of [[Cryptography]] where you have one key that is shared

- Pro: The algorithms are fast
- Con: the key has to be securely shared
# Formal definition
A symmetric encryption key is a triplet of algorithms:
- Key generation (outputs a key $k$)
- Encryption $C \leftarrow Enc_{k}(M)$ 
	- For a key $k$, encrypt a plaintext message $M$ to a ciphertext $C$
- Decryption $M \leftarrow Dec_{k}(C)$
	- Decrypts a ciphertext into a message using the key

![[IND-CPA#Definition]]

# Ciphers
- An algorithm for encrypting or encrypting
- Processes streams in real-time

Types:
- [[#Block ciphers]]
- [[#Stream ciphers]]

> [!NOTE] Design principles
> 1. Confusion: the relationship between the key and the ciphertext should be as complex as possible
> 	- Each ciphertext bit should depend on multiple parts of the key
> 2. Diffusion: each plaintext bit should affect as many ciphertext bits as possible
> 	- Goes both ways

# Block ciphers
- Processes the message in fixed-size blocks
- Pros: better security
	- Keys can be reused
- Use case: blocks of data like files and emails
- Examples: [[DES]], [[Advanced Encryption Standard|AES]]

## Long messages
- Block ciphers need to be used in special modes to support messages longer than their block size
- $M$ may be securely padded until its length is a multiple of $m$ (message length in bits, E.G 128)

### Electronic Code Book mode
AKA ECB mode

- Encrypts each block and concatenates them
- Insecure
	- It's deterministic
	- This means encrypting a block that contains the same data outputs the same ciphertext

![ECB vs CBC mode](https://wiki.bi0s.in/crypto/img/tux.jpg)

### Cipher block chained mode
AKA CBC mode

![CBC mode diagram](https://www.researchgate.net/publication/215783767/figure/fig1/AS:394138559238144@1470981363092/Cipher-block-chaining-CBC-mode-encryption.png)

- Takes in a random initialisation vector $\{0,1\}^{m}$
- [[Bitwise Operations#XOR|XOR]]s each successive ciphertext block with the next
- Concatenate the ciphertexts
- Random, so repeated messages are encrypted differently
	- The initialisation vector determines the strength of this mode

Cons:
- Can't be parallelised because it's sequential
- If a block is lost during transmission, you need to get it before you can decrypt the next one

###  Counter mode
AKA CTR mode

- Initialise a string of bits $ctr = \{0, 1\}^{m}$
	- Referred to as [[Nonce|nonce]]s
	- Must be unique or repeated messages will not be encrypted differently
- Encrypt each $ctr_{i}$ and [[Bitwise Operations#XOR|XOR]] it with block $M_{i}$
- Concatenate the cipher texts

Pros:
- Paralellisable
- You don't need the inverse of the cipher (I.E you don't need the decryption algorithm)
- You can pre-process some operations if you're XORing a message block with a pre-computed cryptographic output

# Stream ciphers
- Processes messages in variable lengths, operating bit-by-bit/byte-by-byte
- Often uses XOR
- RC4, Salsa20, Grain128a
- Pros: faster
- Cons: you can't reuse keys between messages
- Use case: streams of data like wireless connections

# Pseudorandom permutations
AKA PRPs

Block ciphers are built on top of this concept

- A keyed permutation $F_{k}: \{0, 1 \}^{m} \rightarrow \{0,1\}^m$
	- Behaves uniformly at a random permutation
- $\theta: \{0, 1\}^{m} \rightarrow \{0,1\}^m$
	- Can be viewed by a party that doesn't know $k$
