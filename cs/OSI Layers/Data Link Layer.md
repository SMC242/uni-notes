---
tags:
  - OSIModel
---

# Roles
- Gatekeeper to the [[Physical Layer]]
- Addressing
- Structuring the bitstream
- Detecting errors in the bits

# Functions
- Helps devices find each other via addressing
	- Particularly important in the case of wireless networks because they have multiple hosts connecting
	- It's rarer for cables to be multi-access
- Lots of protocols use globally unique addresses, however this is not necessary within a local network
- Makes the bitstream reliable
	- The stream from the [[Physical Layer]] may be corrupted or mistimed
	- The Data Link Layer solves this by putting the bits into frames
	- Then repairs the frames
	- Tries to reduce transmission errors but can't eliminate them entirely

# Framing
Bits are structured into fields. This is how Ethernet does it:
![Ethernet Frame](https://www.flukenetworks.com/sites/default/files/blog/ethernetbacktobasic01_1.png)

The CRC (cyclic redundancy code) contains where errors were detected and if they were fixed

# Error detection
## Parity codes
A method of detecting errors
- Counts the 1 bits
	- Odd number = parity 1
	- Even number = parity 0
	- This is the XOR function
- Compares the parity of the sender and receiver. If they are different, an error has happened

## Checksum
- A method of summarising a file
- If any bytes change, the checksum will completely change

# Error correction
- When an error is detected by the receiver, it tries to correct it on its own
- The receiver may request re-transmission if it can't fix the errors
- Hamming code is one method of fixing errors
	- It mainly fixes single-bit errors. It can fix *some* multi-bit issues but not most

# Synchronisation
- When a network is busy, there will be latency as all the hosts are competing for bandwidth
- Some solutions do not work
	- Each frame can be given a length field (can be corrupted)
	- Timing gaps between frames (latency)
- Instead, a start code is at the beginning of each frame

## Bit stuffing
- Zeroes are inserted after every 5 consecutive 1s
- The receiver removes these
- Then it looks at the seventh bit
	- 0? Start code
	- 1? Corrupt frame
- This is called a binary-level escape code

# Media access control
- Managing who can access the link when

## Collision
- When two hosts try to transmit at the same time
- A system where multiple hosts share a channel is known as a *contention-based system*

Before transmitting, the protocol will check if a collision would happen. This is done by listening to the channel before and during transmission. If there would be a collision:
- Back off and retransmit according to some priority system
- A random back-off delay is used

Protocols are designed to reduce propagation delay (the time taken for a signal to reach its destination). This is because high propagation delay increases the likelihood of collisions
