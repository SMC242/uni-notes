---
aliases: NIDS
---
# Overview
A type of  [[Intrusion Detection System|intrusion detection system ]] that examines network packets
- Examines packets in real time or close to real time

# Sensors
- Inline: inserted into network segments so that traffic must pass through the sensor
	- Can be added to a [[Firewall|firewall]] or [[LAN]] switch
	- Can block an attacker once detected
- Passive: monitor copies of the traffic (traffic doesn't pass through the sensor)
	- More efficient as they don't add an extra step
	- Reduces latency

## Wireless network sensors
- These sensors are capable of analysing wireless traffic (and only that kind of traffic)
- Usually just one part of a NIDS

# Deployment location
There are a few places a NIDS could be deployed within a network:

1. Immediately after the [[Firewall#Layering|external firewall]]
	- Use cases:
		- Detecting attacks from the outside world
		- Finding issues with the firewall policy or performance
		- Can detect attacks on web or [[DNS]] servers
		- Sometimes, the incoming traffic doesn't trigger the IDS, but the outgoing traffic does
	- Still protects the [[Firewall#Layering|DMZ]]
2. Before the external firewall
	- Detects attacks from the internet
	- Resource-intensive because it has to process all traffic into the network
3. Before major network components (E.G resource servers like databases)
	- After an [[Firewall#Layering|internal firewall]]
	- Monitors a lot of traffic - higher likelihood of discovering an attack
	- Can detect unauthorised activity within the organisation's perimeter
		- I.E someone has obtained access who shouldn't have
4. Between workstations or departments
	- After internal firewalls
	- Detects attacks on important systems or resources
	- Can be fine-tuned for specific attack types

> [!tip] Processing burden
> Processing burden can be reduced by watching for specific attacks. This mostly applies to locations 3 and 4

