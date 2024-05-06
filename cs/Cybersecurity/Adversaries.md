# Overview
There are a few categories of attackers that we aim to defend against in cyber-security

# Passive
Goals:
- Identify the sender/receiver of a message
- Find out which messages were sent/received by the same user
- Profile a user's activity
	- Time and volume of messages
	- Who they might be communicating with

Ways that they can view a network:
- Global: observe all communication links in a network and look at the traffic
- Partial: observe only some communication links and traffic
- Local: controls one edge of a network
	- E.G a malicious network node

# Active
Goals:
- The same as a [[#passive]] adversary
- Injecting messages
- Deleting or delaying messages using traffic analysis
- Modifying messages to assist tracing

Limitations:
- Can't break [[Cryptography|cryptographic]] primitives
	- They have to learn critical information like the keys
- Can't see inside nodes they don't control
	- They don't know exactly what security capabilities are in use
