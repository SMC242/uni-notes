---
aliases:
  - DoS
  - DDoS
---
# Overview
AKA DoS attacks

When a service is flooded with requests such that the service's resources are exhausted and cannot respond to normal users

- Attacks on availability

# Categories
- Network bandwidth: spam requests
- System resources: overloading and crashing the server software
- Application resources: spam of valid requests that take a long time to complete

# Examples
## Flood attacks
When the attacker attempts to overload the target server, causing a high discard rate for legitimate traffic. It targets either:
- The network capacity
- The server's ability to handle/respond to traffic

Examples:
- [[Ping Flood]] (network): spamming the server with ICMP packets
- [[SYN Spoofing]] (system): making the server respond to fake requesters
- [[UDP Flood]] (system): spamming with [[UDP]] packets for different ports

## Distributed
See [[#Distributed attacks]]

- [[Reflector Attack]] (network): sending a small request another server to get a bigger one sent to the target
- [[Amplifier Attack]] (network): broadcasting a request to a network to get all hosts inside it to respond to the target
- [[SIP Flood]]

# IP address spoofing
- The attacker forges the source address when sending packets
	- The attacker needs [[Access Control|high-priveleged access]] to the source server
	- They would need to edit the socket interface in the [[Internet Protocol]] [[Protocol Layering|layer]]
- This is a core vulnerability of [[TCP]] over IP
	- TCP doesn't check whether the source address is correct

Advantages:
- The attacker won't receive the responses
	- Random services get them instead ("backscattering")
- Harder to identify
	- You have to trace the [[Routing|route]] that the packets took

# Distributed attacks
AKA DDoS attacks

- Using multiple systems in the attack
- Provides the attacker with more resources
- Harder to trace

## Botnets
- A flock of compromised computers ("zombies")
- *Handler zombies* coordinate other *agent zombies*
	- Requires fewer requests (read: requests) from the attacker
	- I.E the attacker sends a request to 20 handlers instead of all 500 zombies

# Prevention
## Ingress filtering
Countermeasure for [[#IP address spoofing|address spoofing]]

- ISPs know the addresses of their customers
- They can make sure that all packets have valid source addresses
- Consumer ISPs don't do this due to performance constraints

## Acceptance rate
Countermeasure for [[#Flood attacks]]

- Reduce the acceptance rate of ICMP and UDP flood packets
- Usually implemented with rate limiting

## Selecting dropping
Countermeasure for [[SYN Spoofing]]

- Drop entries for incomplete connections
- Only do this when overloaded
- Can have collateral damage
	- Legitimate connections may be dropped
	- Malicious connections are more likely to be dropped

## Block broadcasts
Countermeasure for [[Amplifier Attack]]s

- Block directed broadcasts

## Graphical puzzles
AKA [captchas](https://www.google.com/recaptcha/about/)
Countermeasure for [[#Botnets]]

- Create a randomised visual puzzle that only humans can solve
- [[Machine Learning]] is making this less effective

# Detection
- Detect abnormal traffic by monitoring the network traffic with intrusion detection systems and firewalls 
	- Example service: [Cloudflare](https://www.google.com/recaptcha/about/)
- When an attack is detected, figure out which type it is
- Apply appropriate filters for the attack
- Advantage: you can apply stricter filters without having to apply them all the time

# Response
- Attackers may use DoS attacks to distract organisations from secondary attacks
- Use back-up plans
	- Use overflow servers
- Analyse the attack afterwards to learn how to mitigate it in the future