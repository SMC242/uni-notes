---
aliases:
  - AES
---
# Overview
A standardised [[Symmetric Cryptography|symmetric encryption]] cipher with these characteristics:
- [[Symmetric Cryptography#Block ciphers|Block cipher]]
- Multiple key lengths available: 128, 192, or 256
- Multiple rounds
	- These rounds are successive layers of encryption
	- The number of rounds depends on the key length
		- 128-bit key: 10 rounds
		- 192-bit key: 12 rounds
		- 256-bit key: 14 rounds

Implemented at the hardware level on many [[CPU]]s

> [!WARNING] Note
> This note focuses on AES-128, but will apply to the other key lengths

# Overview
> [!INFO] Key size
> 128 bits is 16 bytes

## Initialisation
1. Arrange 128 bits into a [[Matrices|matrix]] of 16 bits
	1. This matrix is used to create $STATE$, a 4x4 [[Matrices|matrix]] that will be modified at each step
2. The key is arranged into 4 words: $K_{0,}..., K_{3}$
	1. Each key has a 4-byte column
	2. These keys are used in the `KeyExpansion` function to get 44 words $W_{0}, ..., W_{43}$
	3. `KeyExpansion` creates a key for each round by arranging the words into 11 4x4 matrices ("round keys")
		1. E.G $[W_{0}, W_{1}, W_{2}, W_{3}]$

## Rounds
> [!INFO] Reversibility
> All of the functions are invertible.
>
>- The inverse of `AddRoundKey` is just `AddRoundKey`

1. `AddRoundKey(A, B)` takes the [[Bitwise Operations#XOR|XOR]] if the two inputs
	1. At round = 0, `AddRoundKey(STATE, round_key0)`
		1. Adds  [[Symmetric Cryptography#Ciphers|confusion]] by mixing keys with state
2. Rounds 1..9:
	1. `SubBytes` substitutes the state byte-by-byte using a look-up table (`S-box`)
		1. E.G byte $00_{16}$ is swapped for $63_{16}$
		2. $STATE \leftarrow SubBytes(STATE)$ (I.E it mutates the state)
		3. Non-linear because $SubBytes(A \oplus B) \neq SubBytes(A) \oplus SubBYtes(B)$
	2. $STATE \leftarrow ShiftRows(STATE)$: performs a [[Bitwise Operations#Shift|cyclical left shift]] operation on positions 1..3
		1. Adds [[Symmetric Cryptography#Ciphers|diffusion]]
	3. $STATE \leftarrow MixColumns(STATE)$ changes each byte in a column by looking at *all* bytes in the column
	4. `AddRoundKey(key)` is called using the round key
3. Round 10:
	1. $STATE \leftarrow SubBytes(STATE)$
	2. $STATE \leftarrow ShiftRows(STATE)$
	3. $STATE \leftarrow AddRoundKey(STATE, [W_{40}, W_{41}], W_{42}, W_{42}])$
	4. Output $C \leftarrow STATE$

## Decryption
- Use the round keys in reverse order
- Use the inverse of each function 

