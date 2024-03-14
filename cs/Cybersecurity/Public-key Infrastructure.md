---
aliases: PKI
---
# Overview
- [[Public-key Encryption#Overview|Public keys]] can be forged
	- You can't know if a public key was created by the legitimate user
- Solution: get a trusted third party to [[Cryptographic Signing|sign]] and distribute the key
	- The third party is called a "certificate authority" (CA) and issues certificates (the signed keys)
- The infrastructure for issuing certificates is called a PKI

See first:
- [[Public-key Encryption]]
- [[Cryptographic Signing]]

# Protocol
## Third party
1. Create a public-private key pair $sk_{c}, pk_{c}$

## Usage
1. Party 1 sends their ID $ID_{A}$ and public key $pk_{A}$ to the third party
2. The third party signs the key and sends it to party 1
	- $cert_{A} \leftarrow Sign_{sk_{c}}(ID_{A} || pk_{A})$
3. Party 1 distributes their ID, public key, and certificate via some public channel
4. Party 2 verifies the signature
	- $Vrfy_{pk_{c}}(ID_{A} || pk_{A}, cert_{A}) \stackrel{?}{=} 1$
5. 
	

> [!TIP] Concatenation
> $||$ represents string concatenation


# Distributing CA keys
- Problem: how does party 2 get the CA's public key $pk_{c}$?
- Solutions:
	- Distribute it with software (browsers, operating systems)
	- Physically give it to them
		- Used when an organisation has its own CA

## Verifying certificate requests
A CA can know that a public key is legitimate if either:
- The ID provided is valid
	- Example IDs: an email address or company domain name
- Party 1 physically presents the public key and their ID
	- Used when organisations own their CA

## CA hierarchy
- There has to be a single "god" CA that everyone recognises
- Problem: there would be too much traffic for one server to handle
- Solution: have a hierarchy of CAs
	- The root CA grants certificates to some intermediate CAs
	- The intermediate CAs sign end-user certificates

> [!NOTE] Advantages
> - The root CA can have a higher level of security
> 	- Not connected to the internet
> - Granularity: you can deploy many CAs across the world and have differing levels of security 


## Validity
- Certificates expire after some time and can be revoked
	- In case a user's private key is stolen or an employee leaves a company
- Certificates have a validity period and a serial number
	- The serial number is used to ask the CA if the certificate was revoked
- 