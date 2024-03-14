---
tags: Protocol
---
# Overview
- Stands for [[Transport Layer]] Security protocol
- Used in [[HTTPS]]
- Provides:
	- Confidentiality via [[Cryptography|encryption]]
	- Integrity via [[Message Integrity|MACs]]
	- Server and client authentication with [[Public-key Encryption]]
		- Client authentication is optional

# Protocols
## Handshake
- When the server and client [[Authentication|authenticate]] each other, negotiate encryption and [[Message Integrity|MAC]] algorithms, and share [[Cryptography|cryptographic keys]]
- Similar to [[TCP#Handshake|TCP handshake]]

1. Client sends `ClientHello` with the following information:
	1. Its latest TLS version
	2. A random [[Nonce]] $n_{c}$
	3. A session ID
	4. The cryptographic algorithms the client supports (ciphersuite)
	5. The [[Text Compression|compression algorithms]] the client supports
2. The server responds with `ServerHello` with:
	1. The highest TLS version shared by the server and client
	2. A ciphersuite and compression algorithm chosen from the client's options
	3. A nonce $n_{s}$
3. The server sends its public key $pk_s$ and a [[Public-key Infrastructure|certificate]]
	1. If more information is required, a `ServerKeyExchange` message will be sent. Used for [[Public-key Encryption|public key exchange protocols]] like [[Diffie-Hellman Key Exchange Protocol|Diffie-Hellman]]
4. The server sends `ServerHelloDone`
	- Marks the end of the negotiation phase
	- The client will verify this 
5. The client sends `ClientKeyExchange` with:
	1. A premaster secret which might be an encrypted random string or a [[Diffie-Hellman Key Exchange Protocol|Diffie-Hellman value]]
6. The server and client compute:
	1. The premaster secret by decrypting the string or completing the [[Diffie-Hellman Key Exchange Protocol#Protocol|Diffie-Hellman exchange]]
	2. Use the premaster secret and random nonces $n_c$, $n_s$ to derive the keys for [[Symmetric Cryptography|symmetric encryption]] $k_{sym}$ and [[Message Integrity|MAC]] $k_{mac}$
7. The client sends [[#ChangeCipherSpec]] and `Finished`
	1. `Finished` contains a hash of all previous messages, [[Authentication|authenticated]] and encrypted using the keys
8. The server sends `ChangeCipherSpec` and `Finished` if the MAC from `Finished` was successfully verified
9. The client decrypts the `Finished` message and verifies its MAC

![[TLS Handshake.png]]

## ChangeCipherSpec
- A message saying that subsequent messages will be encrypted with the previously negotiated keys

## Record 
- Encrypt a message with the negotiated keys and protect the messages' integrity

## Alert
- Used to throw errors  ("fatal") and warnings
	- Error examples:
		- Decryption failed
		- Handshake failure
		- Certificate revoked

## Heartbeat
- Periodically tell the recipient to keep the connection alive
	- Used when there hasn't been activity for a while
- Workaround for [[firewall]]s closing idle connections