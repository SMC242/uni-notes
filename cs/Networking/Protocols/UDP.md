---
tags: Protocol
---
# Overview
- Bare-bones protocol built on top of the [[Internet Protocol]]
	- Connectionless and best-effort
		- This makes it unreliable
- Framed
- No congestion control
- Comparable to throwing darts at a wall and not checking if they hit

# Format
- 16-bit port number used as a service identifier

# Use case
- Inelastic applications
- Peer-to-peer communication
	- E.G voice chat ("VoIP" - Voice over Internet Protocol)

# C API
A similar API to [[TCP Sockets]] has been made standard

- UDP ports don't overlap with TCP
- Both peers `bind` to the same port
- No `connect` or `accept` call as there is no connection
	- `connect` is instead used to set the default destination for subsequent `sendto`s
- `sendto` sends a datagram
- `recvfrom` gets a datagram and the sender's address (important if something will be sent back in your protocol)
- Each UDP datagram is one [[Internet Protocol#IP packet format|IP packet]]
	- Might still be fragmented if using [[Internet Protocol#IPv4|IPv4]]

# Unreliability
- Pakcets might be dropped, delayed, reordered, or duplicated
- The application must correct these errors
	- Usually a sequence number is added to the protocol
- The application should be able to recover from lost packets
	- Video streaming uses intermediate and predicted frames

## Application responsibilities
The application is expected to implement many features

- Add sequencing, reliability, and timing
- Usually sequence numbers, acknowledgements, re-transmission, and timing recovery are added

### Congestion control
- Whatever algorithm is used should be fair to TCP
- Error-prone as application developers might forget about corner-cases
- 