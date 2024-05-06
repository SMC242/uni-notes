# Overview
AKA IEEE 802.11 WLAN

A standard protocol created by the [IEEE](https://www.ieee.org/) for wireless communication within a LAN. Built on top of IEEE 802

Specifies details over 3 layers:
- [[#Physical layer]]
- [[#Medium Access Control layer]]
- [[#Logical Link Control layer]]

See first:
- [[Protocol Layering]]
- [[Physical Layer]]

See also:
- Its successor, [[RSN]]

# Physical layer
- The [[Physical Layer]] is augmented with standard frequency bands and antenna specs

# Medium Access Control layer
- Data is received from higher layers as MSDUs (MAC service data units)
- When transmitting, assembles data into [[Data Link Layer#Framing|frames]] called MPDUs
- When receiving, disassembles frames
	- Recognises the address
	- Checks for errors
- Manages access to LAN transmissions

## MPDU
- Stands for MAC Protocol Data Unit
- Specified in IEEE 802

Fields:
- MAC control: protocol control information
	- Addressing
	- Error detection
	- Flow control
- Source and destination [[MAC Addresses]] within the LAN
- MSDU (MAC Service Data Unit): the payload
- [[CRC]]

# Logical Link Control layer
- Handles [[Protocol Layering#Data link layer|flow]] and error control
- Re-transmits frames that failed to transmit
	- E.G wasn't received or the CRC was invalid
	- The detection happens at the [[#Medium Access Control layer]], not here!

# Basic service set
AKA BSS

- The wireless devices using the same MAC protocol and sharing the same wireless medium
	- Competing for access to bandwidth
	- The devices are known as stations ("STAs")
- Might be isolated
- Might be connected to a "backbone" distribution system (DS) via an AP
	- The DS could be a [[cs/Networking/Signals#Switching within networks|switch]], a wired network, or a wireless network
		- They connect multiple BSSes so that you can move around the area of the LAN and still have a connection
	- In this situation, the AP is a [[Network Bridge|bridge]] to the DS and a relay point
		- Transfers MAC frames from the BSS to the DS
			- The frame is "relayed by the AP over the DS" to the destination station


## Extended service set
AKA ESS

- When multiple [[#Basic service set|BSSes]] re connected with a DS
- Looks like a single *logical* LAN at the [[#Logical Link Control layer|LLC layer]]
	- I.E can be treated as one LAN but it's actually multiple

# Services
## Distribution
- Distribution is the service for sending MPDUs between BSSes
- IEEE 802.11 doesn't specify how the frames are transferred through a DS

## Integration
- Transferring data between stations on a LAN and a station on an integrated LAN
	- Integrated LANs are wired LANs that are physically connected to the DS and logically connected to the other LAN
- Handles translating the addresses between the two LANs

## Association
- Creating an association between a station and an AP
	- Tells the AP the stations' identity and address
- Once complete, the station is now allowed to transmit/receive through the AP

## Reassocation
- Transfers an existing association from one AP to another
- Used when a device moves out of the range of one AP within a network into the range of another AP

## Disassociation
- Terminating an association
- Can be sent by either party
- Should be sent when an a station leaves an ESS or shuts down

