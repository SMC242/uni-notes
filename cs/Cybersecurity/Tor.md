---
aliases:
  - The Onion Router
---
# Overview
The Onion Router is a [[routing]] system that protects [[anonymity]] for senders and receivers. 

- Each router is called an OR (Onion Router)
- Based on [[TLS]]
	- Protects the connection data
	- Prevents modification of data during transfer 
	- Prevents impersonating ORs

> [!INFO] Key Information
> - The Tor client selects a 3-router route
> 	- By doing these 3 hops, anonymity is ensured
> 	- Relay 1 knows the origin, but not the destination
> 	- Relay 2 knows the destination, but not the origin
> 	- Relay 3 knows the destination
> - The data starts with 3 layers of encryption. One layer is removed per hop

> [!WARNING] Who Does It Protect Against
> Tor protects anonymity against [[adversaries]] that can:
> - Observe some but not all of the network's traffic
> - Has compromised some but not all routers
> - Can replay traffic
> - [[Denial of Service|Deny service]] trustworthy routers so that traffic is sent through compromised routers
> - [[Denial of Service|Deny service]] to users to test if traffic in another part of the network stops
>
> Tor can't protect against [[adversaries]] that:
> - Have [[Adversaries#Passive|have global access]] to the network
> - Can use end-to-end timing attacks
>
> Overall, an adversary would need to have access to the whole Tor network to breach anonymity

# Cells
- The PDU (Protocol Data Unit) is called a cell
- Has a header containing:
	- A command which says what to do with the payload
		- *Create*/*created* cells create a new anonymous route (called a "circuit")
		- *Relay* cells carry end-to-end stream data
		- *Destroy* cells tear down a circuit
	- The circuit identifier

# Creating a circuit
See first:
- [[Diffie-Hellman Key Exchange Protocol]]
- [[RSA Encryption]]

Setup:
- The client finds the list of addresses and public keys for the ORs
	- These are found in a public directory (maintained by the Tor servers)
- The client picks three relay nodes (guard, middle, exit)

![3 relays diagram](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgTan7UG9iFVLMnRqKx8-adgQyRQ80fNPbHmVu4uYIyj8GAfTkDYByrNn5lQH4drQn-D4C5g_-HFJ-JyVmObkeYPi4fUocnAhcU-HgLk9PRD4r9Ezc2WYtotnvT1W_BsvZgU5QAz9Ex8IP7/s728/Tor+software.png)

Construction:
1. [[Diffie-Hellman Key Exchange Protocol|Diffie-Hellman key exchange]] with the guard initiated wit a *create* cell
	- The cell contains $C_{C \rightarrow G} = RSA_{guard}(g^{x_1})$
2. The guard decrypts to get $g^{x_{1}}$ and sends back $g^{y_{1}}$ and a hash of $K_{CG} = (g^{x_{1}})^{y_{1}}$
	- $C_{G \rightarrow C} = (g^{y_{1}}, H(K_{CG}))$
3. The client computes $K_{CG} = (g^{y_{1}})^{x_{1}}$ and compares it with the hash $H(K_{CG})$
	- This marks the end of symmetric key negotiation *with the guard*
	- The client still needs to negotiate with the middle and exit
4. The client sends a *relay* cell to the guard with a symmetric encryption: $E_{C \rightarrow G} Enc_{K_{CG}} (extend, middle, RSA_{middle}(g^{x_{2}}))$
5. The guard decrypts $E_{C \rightarrow G}$ to get $RSA_{middle}(g^{x_{2}})$
	- The guard will act as a proxy between the client and middle
6. The middle negotiates as before and a *created* cell is sent back to the guard with $g^{y_{2}}$ and $H(K_CM )$
7. The guard sends a *relay* cell to the client with $E_{G \rightarrow C} = Enc_{K_{CG}} (extended, g^{y_{2}}, H(K_{CM}))$
8. The client decrypts $E_{G \rightarrow C}$ to get $g^{y_{2}}$ and$H(K_{CM})$
	- The client computes $K_{CM} = (g^{y_{2}})^{x_{2}}$ and compares it with the hash
	- Now the symmetric key negotiation with the middle is complete
9. The same process is repeated again with the exit node to negotiate $K_{CE}$
	- The guard relays the messages from client to middle
	- The middle is the proxy

> [!info] Summary
> - The client uses the [[Diffie-Hellman Key Exchange Protocol]] to negotiate keys with each node
> - *created* cells are sent to the proxy and the proxy sends a *relay* cell to the previous node (either a proxy or the client)
> - Result: the client has a key for each edge in the chain
> 	- Client --> guard: $K_{CG}$
> 	- Guard --> middle: $K_{CM}$
> 	- Middle --> exit: $K_{CE}$
>
> ![Flow diagram for circuit construction](https://www.researchgate.net/publication/313951935/figure/fig3/AS:557209840033793@1509860586144/TOR-Circuit-Making-Process-OR-is-abbreviation-of-Onion-Routing.png)

# Communication
- Communication happens using 3 layers of encryption
	- $C \leftarrow Enc_{K_{CG}}(Enc_{K_{CM}}(Enc_{K_{CE}}(M)))$
- After each hop, the OR decrypts its layer of encryption
	- This is why Tor is called The Onion Relay- it *peels off a layer of the onion* at each hop
- The recipient thinks that the traffic came from the exit node
- The recipient responds and the layers of encryption are applied as the response goes through the relays
	- Result: $C \leftarrow Enc_{K_{CG}}(Enc_{K_{CM}}(Enc_{K_{CE}}(R)))$