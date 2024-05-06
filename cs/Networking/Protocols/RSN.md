# Overview
AKA Robust Security Network

- An improvement over [[WLAN]]
	- Adds [[Authentication]], [[Access Control]], and [[Message Integrity]]
	- Uses an authentication server (similar to the [[Kerberos]] system) to mutually authenticate the station and AP
	- Generates *temporary* [[Cryptography|keys]]

See first:
- [[WLAN]]

# Phases
1. [[#Discovery phase]]
2. [[#Authentication phase]]
3. [[#Key management phase]]
4. [[#Protected data transfer phase]]
5. [[#Connection termination]]

## Discovery phase
- The AP uses "beacons" and "probe responses" to advertise its security policy
	- These messages are used by a station to find an AP to associate with
- Negotiates security algorithms
- 

## Authentication phase
- The station and AS prove their identities
	- Messages are forwarded via the AP
	- Non-authentication traffic is blocked until complete

## Key management phase
- Keys are generated

## Protected data transfer phase
- Station-to-station transmission can now occur
- Note: security is only provided between the origin station and the AP
	- If the stations are in the same network, this will be secure
	- If the stations are in an ESS, there will be security within *each* BSS but not in the DS --> not secure station-to-station
	- If the destination station is on a wired network, only origin station --> AP is secured

## Connection termination
- The connection is destroyed