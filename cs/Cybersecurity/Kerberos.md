---
tags:
  - Authentication
  - Protocol
---
# Overview
- An early third-party authentication service from 1988
- Newer protocols like [[OAuth]] have been developed

# Entities
- A user wants to [[Authentication|authenticate]] to a service (Server Service, SS) via a client
- The client interacts with the Kerberos servers:
	- Authentication server (AS)
	- Ticket-granting server (TGS)

# Process
Useful explanation: https://youtu.be/5N242XcKAsM

1. User logs into client, sends request to SS
	1. The client sends a cleartext message to AS including the user ID and asks for a ticket-granting ticket (TGT)
2. AS checks database for key (hashed password, $H(P)$) and responds with two messages encrypted with $H(P)$
	1. Client/TGS session key $K_{C}^{TGS}$
	2. A TGT (includes $K_{C}^{TGS}$, user ID, timestamp, validity period, client [[Internet Protocol#IP address|IP address]]) encrypted with AS/TGS shared key $K_{AS}^{TGS}$
3. Client asks user for password $P$, derives key $H(P)$, decrypts incoming messages
4. If successful, the client sends 2 messages to the TGS:
	1. ID of the SS and TGT encrypted with $K_{AS}^{TGS}$
	2. An authenticator (a composition of the user ID, client [[Internet Protocol#IP address|IP address]], timestamp) encrypted with $K_{C}^{TGS}$
5. TGS decrypts TGT, derives $K_{C}^{TGS}$. Decrypts and derives authenticator. If the IDs and IP addresses match, the TGS knows the ticket sender is the ticket's true owner
6. TGS sends 2 messages to the client encrypted with $K_{C}^{TGS}$
	1. Client/SS session key $K_{C}^{SS}$
	2. Service-granting ticket (includes $K_{C}^{SS}$, user ID, client [[Internet Protocol#IP address|IP address]], validity period) encrypted with TGS/SS shared key $K_{TGS}^{SS}$
7. Client decrypts and derives $K_{C}^{SS}$ and reusable service-granting ticket. Obtains necessary information to authenticate itself to SS
8. Client sends 2 messages to SS:
	1. Service-granting ticket
	2. Another authenticator (includes user ID, client [[Internet Protocol#IP address|IP address]], timestamp)
9. SS decrypts service-granting ticket,  derives $K_{C}^{SS}$, decrypts and derives authenticator. If the IDs and IP addresses match, authentication is successful
10. [Optional] For mutual authentication, the server can respond with the timestamp (sometimes incremented by 1) from the authenticator,  encrypted by $K_{C}^{SS}$. The client can check the correctness of the timestamp

# Problems
- AS and TGS are single points of failure
	- If they go down/are compromised, everyone else is too
- The clocks of SS, AS, and TGS must be synchronised because timestamps are used