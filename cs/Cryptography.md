# Overview
- Encryption is the practice of making text unreadable
- It is two-way unlike [[Hashing]], so it can be decrypted
- A key is used to decrypt the text

# Terms
<dl>
	<dt>Cipher-text</dt>
	<dd>Encrypted text</dd>
	<dt>[Secret] key</dt>
	<dd>The key used to encrypt and/or decrypt the text. Can be thought of like a seed in a randomiser</dd>
</dl>

# Symmetric

^0f8478

- The same key is used for encrypting and decrypting the text
- The cipher-text is binary (usually base64)
<ul class="breakdown">
	<li class="pro">Fast (may be used for bulk encryption)</li>
	<li class="pro">Simple to implement</li>
	<li class="con">The key has to somehow be distributed privately</li>
</ul>

# Asymmetric

^92b687

- Uses 2 keys:
	- Public key: openly distributed (it doesn't matter if it's compromised)
	- Private key: must be secret
- The public key is used for encryption and the private key is used for decryption
- The public key is usually published in some directory

<ul class="breakdown">
	<li class="pro">Solves the distribution issue</li>
	<li class="con">Slow</li>
</ul>

# Hybrid
- Uses [[#Symmetric]] and [[#Asymmetric]] 
- Creates a temporary session key with [[#Symmetric]] cryptography
- Distribute a public-key system to distribute the session key
	- This is fast because the session key is small
- The payload is encrypted with [[#Symmetric]] cryptography and can be decrypted with the session key
- Used in [[TLS]]

<ul class="breakdown">
	<li class="pro">Secure</li>
	<li class="pro">Fairly fast - a middle-ground between the two methods</li>
	<li class="con">Not as fast as symmetric cryptography</li>
</ul>