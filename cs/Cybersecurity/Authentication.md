# Overview
Authentication is the practice of verifying the claimed identity of a user. It has two steps:
1. Identification: presenting an identifier to the system
2. Verification: Presenting or generating proof of a binding between the entity (I.E user) and the identifier

# Authentication methods
There are a variety of high-level authentication strategies:
- Something the user knows (passwords, PINs, security questions)
- Something the user has (key-cards, smart cards, keys)
	- Called tokens
- Something the user is - biometrics (fingerprint, retina, facial recognition)
- Something the user does - dynamic biometrics (vocal recognition, handwriting, typing rhythm)

## Multi-factor authentication
AKA MFA

- When the user presents $\ge 2$ pieces of evidence
- Typically a combination of traditional methods
- One-time passwords (OTPs) can be used
	1. The user enters their password
	2. A temporary password is generated and sent to the user via SMS, email, voice call, or an authenticator app
		- The authenticator app may be secured with biometrics or other strategies

# Password-based authentication
![[Passwords]]
# Tokens
- Physical authentication methods

## Memory cards
- Store data, does not process
- Uses a magnetic strip on the back
	- Stores a security code
- User provides both the card and a password/PIN

## Smart cards
- Tokens with embedded microprocessors
- Provides an electronic interface
	- Contact: card inserted into smart card reader. Transmission happens using physical contact points
	- Contactless: close proximity to reader. Card and reader communicate via radio waves

### Protocols
- Static: user authenticates themselves to the token, then the token authenticates the user to the system (user --> token --> system)
- Dynamic password generator: token generates unique password periodically. Password is entered into system either manually or electronically via the token
- Challenge-response: the system generates a challenge and the smart token responds with the solution. The response is generated using some cryptographic operation (E.G shared secret key, [[Cryptographic Signing|digital signature]])

## Electronic identity cards
- Smart cards verified by national government
- Used for:
	- ePass: a digital representation of the cardholder's identity
	- eID: provides identity record to authorised services (cardholder must give permission)
	- eSign: stores a private key and certificate verifying the key. Used for [[Cryptographic Signing]]

# Biometric authentication
Authentication using physical characteristics

- Facial characteristics
	- Location and shape of key facial features (eyes, eyebrows, nose, lips, chin)
- Fingerprints
- Hand geometry
	- Shapes, lengths, widths of fingers
- Retinal pattern
	- The pattern of veins underneath the retinal surface
- Iris
- Signature
	- The user's unique style of handwriting
	- Multiple samples will not be identical as people don't write exactly the same each time
- Voice
	- Vocal patterns
	- Variation between samples over time

## Cost vs accuracy

![Cost vs accuracy of various techniques](https://slideplayer.com/slide/17625589/105/images/31/Figure+3.7+gives+a+rough+indication+of+the+relative+cost+and+accuracy+of+these.jpg)

## Decision threshold
- Biometrics are not exact due to variance over time
- The decision threshold decides how much two biometric samples must match to be accepted
- Reducing the threshold makes the system easier to use, but more likely to have false-matches
	- Trade-off between accuracy (read: security) and ease of use (lesser security)
- Applications demand different levels of accuracy
	- Situations where the cost of a false-match is costly (E.G critical data) need high accuracy
	- Forensics would want to maximise possible candidates, so lower non-match rate is desirable

![Decision threshold](https://www.researchgate.net/publication/269588602/figure/fig1/AS:411237537075203@1475058078006/The-genuine-and-impostor-score-distributions-for-a-typical-biometric-matcher.png)

# Remote authentication
- Authenticating over the wire introduces extra attack vectors
	- Intercepting messages and replaying them

## Random challenges
- When servers respond, it sends back a random challenge
- The client responds with the solution using some cryptographic function
- Prevents replay attacks

See: [[Nonce]]

## Kerboros protocol
![[Kerberos]]