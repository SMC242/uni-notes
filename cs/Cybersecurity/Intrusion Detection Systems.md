---
alises: IDS
---
# Overview
Systems that detect [[Access Control|unauthorised access]]  by monitoring computers and networks

## Motivation
- The sooner attackers are detected, the less damage they can do
	- Ideally, before any damage is done
- Deterrent
- Helps collect information about intrusion techniques so that vulnerabilities can be fixed

## Principles
- The behaviour of an attacker is sufficiently different from a normal user
- There will be some overlap
	- Try to strike a balance between strictness and looseness so that false alarms are not annoying

## Base rate fallacy
- If the number of intrusions is small compared to the number of legitimate uses, the false alarm rate will be high

> [!EXAMPLE]
> If:
> - 1% of traffic are attacks
> - 90% of intrusions will be detected
> - 10% of legitimate uses are classified as attacks
>
> Then 92% of alarms will be erroneous

# Components
- Sensors: collect data from areas of the system that could have evidence of intrusions
	- Network packets
	- Log files
	- System call traces
- Analysers: pull data from sensors and figure out if intrusion is occurring
- User interface: a dashboard for viewing the status of the system and controlling it

# Categories
- [[#Host-based IDS|Host-based]] (HIDS): monitors single hosts and events inside them
	- Process identifiers
	- System calls
- Network-based: (NIDS): monitors network traffic
	- Network segments
	- Devices
	- [[Network Layer]], [[Transport Layer]], and [[Application Layer]] protocols
- Distributed/hybrid: combines HIDS and NIDS into a central analyser

# Analysis
## Anomaly detection
- A statistical method
	- May use [[Machine Learning]]
- Collecting data about legitimate users and marking unusual behaviour as an attack
- Good for detecting \ero-day attacks
- Difficult
- High false alarm rate

## Signature/heuristic detection
- Detection based on data patterns and/or heuristics
- Cheap
- Widely-used
- Reduces false positives
- Can only identify known attacks

# Host-based IDS
- Can detect internal and external intrusions
- May use anomaly, signature, or heuristic approaches

## Sensors used
- System call traces: a record of the sequence of system calls each process uses
- Audit logs
- Integrity [[checksums]]: a [[Hashing|hash]] added to packets on transmission that is verified when received
	- Critical files are scanned for changes

## Analysis approaches
- Anomaly-based:
	- Check how programs access core [[kernel]] fuctions
	- Verify [[checksums]] on key files
		- Can't detect changes made to processes once they're running (E.G editing memory)
- Signature/heuristic
- Distributed HIDS
	- Used by organisations to defend[[LAN]]s
	- Coordinates IDSes across the network

# Network-based IDS
- Examines packets in real time or close to real time

## Network sensors
- Inline: inserted into network segments so that traffic must pass through the sensor
	- Can be added to a [[Firewalls|firewall]] or [[LAN]] switch
	- Can block an attacker once detected
- Passive: monitor copies of the traffic (traffic doesn't pass through the sensor)
	- More efficient as they don't add an extra step
	- Reduces latency

### Wireless network sensors
- These sensors are capable of analysing wireless traffic (and only that kind of traffic)
- Usually just one part of a NIDS

## Deployment
TODO: 40-44