---
tags: Protocol
---
Stands for Network Address Translation

# Address translation
- Makes changing IPs easier
- Rewrites packet headers to a different [[Internet Protocol#IP address|IP address]]
- This means that a private network can be hidden behind a single public IP address

# TCP
- When connections go out of the network, the data needs to be sent periodically or the NAT will time out
	- Every 2 hours or so
- This is because it keeps the destination in state
- It doesn't keep state for incoming connections because it doesn't know where to forward the connections
	- Can be worked around with port forwarding

# UDP
- NATs give UDP short timeouts because they don't know when the communication has ended due to no connection
	- Send keep-alive messages every two minutes or less, sometimes every 15 seconds
- UDP NATs are less strict than TCP NATs

# Port forwarding
- Only necessary if the server is behind a NAT
- This explicitly tells the NAT where to send packets for a given port

# Why use NATs?
- ISPs can effectively have more addresses because they don't need to give each network its own prefix
- The network doesn't need to be given a new IP when moving between ISPs
	- Applications often hard-code IPs instead of [[DNS]] names which is bad practice