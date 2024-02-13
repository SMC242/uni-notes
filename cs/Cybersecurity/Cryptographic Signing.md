# Signatures
- Attach an encrypted summary of the data before transmitting it
	- [[Hashing|Hash]] it
	- Encrypt it with the [[Cryptography#Asymmetric|private key]]
- This can then be checked on the other side
	- Hashing the received data, then comparing it with the received signature
	- The received signature can be decrypted with the [[Cryptography#Asymmetric|public key]]