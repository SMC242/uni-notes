# Overview
- Reliable stream built on top of the [[Internet Protocol]]
- Has congestion control
- It's an ordered byte stream (no framing)
	- The application is expected to enforce structure

# Format
- 16-bit port number (service identifier)
- `SYN` and `ACK`: shows connection progress
	- `SYN`cronise
	- `ACK`nowledge
	- Makes the protocol reliable
- `seq`: random sequence number
	- To handle delayed packets or restarting a host (*makes it more robust*)
- `FIN`: used to signal ending the connection

# Handshake
- AKA "3 way handshake"
- The first packet has `SYN = 1`, `seq` is a random number
- The reply has `SYN = 1`, `ACK = seq`, `seq` set to a new random number
- Response with `ACK = ` the new `seq` is returned
- Ending the connection is done with a similar handshake

![TCP handshake diagram](https://media.geeksforgeeks.org/wp-content/uploads/TCP-connection-1.png)

# Further communication
- `seq`  indicates how many bytes have been sent because the `seq` and `ack` will effectively count up
	- The `ACK` sent back indicates which byte the receiver is expecting back
	- Multiple packets are in transit at once
- If a packet is lost, the responses will show duplicate `ACK`s
- TCP will retransmit lost packets automatically

# Loss detection
- A "triple duplicate `ACK`" occurs, but later packets were still received
	- Confusingly, this means that 4 `ACK`s in a row were the same
- If the responses stop coming, the receiver or network path has run into an issue

## Handling delay
- If [[Packets]] are delayed, the packets may be reordered and that causes duplicate `ACK`s
- This is why a triple duplicate is the criteria for re-transmission, not a double
- Makes an assumption: the delay isn't too big

# Use case
- Elastic applications