---
tags:
- OSIModel

---
# Roles
- Delivering data across multiple [[Data Link Layer]] hops and different tech
- Transmission across many autonomous systems

# Autonomous systems (ASes)
- A network that is administered separately from others
- This means that they choose their own technologies and policies (what kind of traffic they allow)

# Building an internet
- A common end-to-end network protocol is required
	- This needs to provide easy access to to the [[Transport Layer]]
- Gateway devices (routers)
	- To implement the network protocol
	- To abstract away differences in link layer tech
	- Attempts to do as little translation as possible

![[Internet Protocol]]

# Addressing
2 purposes: 
- Provide an identity for a host
- Show the location of a host

## Identity
- The network layer is responsible for providing consistent addresses
	- Hides issues of multi-homing (having multiple addresses) and mobility (moving)
- The network layer uses the location of the host to create its address
- A database is used within the network to keep track of host identities and their addresses
## Location
- Phone numbers have regional indicators to tell operators where to route traffic
- Multi-homing and mobility is handled by higher layers
	- [[Transport Layer]] implementations break when the host moves, so the [[Application Layer]] may handle it instead

## Hierarchical addressing
- Phone numbers use this
- The regional indicator groups addresses by country
	- Shows where the host is in the network topology
- Very rigid
- Flat addressing is not scalable due to this lack of direction

## Binary vs textual
- Textual is human-readable
- Binary is faster
- Which is used depends on the use-case



