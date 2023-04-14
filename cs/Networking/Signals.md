# Types of signals
- Analogue
	- Continuous
	- The language of physical data
	- Can be corrupted with noise
	- 
- Digital
	- Uses a limited set of "symbols" E.G 1 or 0
	- Computers use binary
	- Networks will use other symbol schemes like 16 or 64 bits
	- The language of computers
	- Rate of symbols per second is the "baud rate"

# Defining a network
- Channels transfer signals
	- Via cables or radio (read wireless)
- A channel that carries a signal is known as a link
- A link connections one or more hosts together
- A network is a graph of links
- The devices that connect links are called switches or routers
	- The name depends on how the network is set up

# Switching within networks
## Circuit switched networks
In this configuration, if a link is shared and one host is using it, the other will be blocked. Phone networks use this

- Issues: blocking
- Advantages: full bandwidth guaranteed 
![[circuit_network.png]]

## Packet switched networks
Messages are chopped into [[Packets]].  This avoids the blocking issue [[#Circuit switched networks]] have. The Internet uses this

- Issues: bandwidth is shared
- Advantages: no blocking
![[packet_network.png]]

## Virtual circuits
- A way of moving data over a packet-switched network (I.E it is simulating a circuit-switched network)
- It gets this name because it looks like there's a dedicated physical link between the source and destination

# Types of links
## Point to point
- Unidirectional between two hosts

## Multi-access
- Multiple hosts using it
- Bidirectional