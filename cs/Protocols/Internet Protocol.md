---
aliases:
  - IP
---

# Roles
- The protocol for communication on the internet
- Addressing, routing, fragmentation, reassembly

# Hourglass model
- IP is the protocol that handles interfacing between the upper and lower layers
- Exists in the [[Network Layer]]
- Links the [[Transport Layer]], [[Data Link Layer]], and [[Physical Layer]]
![Internet Protocol technology diagram](https://scx2.b-cdn.net/gfx/news/hires/2011/howtheintern.jpg)

## Why one protocol?
- There's so many transport and application layer protocols that it's not possible for lower layers to support them all
- IP acts as a standardised way to go between the two sections

# IP Service Model
- Connectionless
	- Just send [[Packets]] without establishing a connection
- [[Best Effort Servicing|Best effort]]
- It's simple to simulate a circuit-switched system with a packet-switched system
	- See [[Signals#Switching within networks]] for more information on circuit and packet systems

# Versions
- The Internet Protocol has been through two main versions
	- An upgrade was needed to handle the growing number of hosts on the internet
## IPv4
- The most common version
- 32-bit addresses
- It's at end-of-life
	- All addresses have already been assigned to [[Inter-domain Routing#Algorithm goals|RIRs]]

## IPv6
- The standard being slowly adopted
- 64-bit addresses
- Simplified packet structure
- Dropped a lot of features

![[DNS]]

# IP address
- The unique address of your computer on a network
- Used by the internet to know where to send data
- It's possible to change it but this doesn't make you unidentifiable
- Not all hosts that have an IP address can be reached. Firewalls may prevent data from being received
- [[Network Layer#Hierarchical addressing|Hierarchical]]
	- Using location
- If a device has multiple network interfaces, each interface will have its own IP address

### Local IPs
- Only visible within the local network

## Formats
### IPv4
`X.X.X.X`
or
`XX.XX.XX.XX`
where the `X`es are decimal numbers

### IPv6
`XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:XXXX`
where the `X`es are hexadecimal digits

### Fields
- Addresses are split into network and host fields
- netmask: the number of bits in the network field

#### Special addresses
- A network's address has the host field set to 0
- A broadcast address has all host bits set to 1
	- Used for sending data to all hosts in the network

# IP packet format
## Basic info
- Source address
- Destination address
- IP version
- Length

## Fragmentation
- The [[Data Link Layer]] has a maximum packet size (MTU)
- IPv4 routers fragment [[packets]] that are bigger than the MTU
	- The More Fragments (MF) bit is set if the packet was fragmented
	- If the Do Not Fragment (DF) bit is set, the packet is discarded
- Also has a fragment offset relative to the first packet
- IPv6 dropped support for fragmentation

## Loop protection
- To prevent a packet from circling in a loop forever, a counter is added to each packet (the *forwarding limit*)
	- The initial value is arbitrary
- Every time a packet is forwarded, that counter is reduced
- Makes a key assumption: the width of the network is less than the forwarding limit

## Header checksum
IPv4 headers have a checksum, while IPv6 doesn't. IPv6 assumes [[packets]] are protected in the [[Data Link Layer]]
- Protects the IP header exclusively
	- Higher layers must protect the payload

## Protocol identifier
- The ID of which [[Transport Layer]] protocol was used
	- Used to pass the packet onto the desired protocol

Some IDs:
- TCP = 6
- UDP = 17
- DCCP = 33
- ICMP = 1

Here is the full list: http://www.iana.org/assignments/protocol-numbers/