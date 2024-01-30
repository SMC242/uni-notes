# Overview
A type of [[Denial of Service]] attack where requests are sent to an intermediate server that forwards them to the target

# Description
1. Attacker sends packets to a known service (the *intermediary* or "reflector)
	1. The packets have a [[Denial of Service#IP address spoofing|spoofed address]] for the target
2. The intermediary sends a **larger** response to the target

> [!tip] Analogy
> Imagine a satellite dish
![Satellite dish](https://qph.cf2.quoracdn.net/main-qimg-24b51c6df78f13a2bb5b38558a737f3d)

## Advantages
- The attacker doesn't need lots of resources because the reflector does most of the work
- No backscatter traffic
	- Harder for researchers to investigate the attack

# Common types
- [[UDP]] services are typically used
	- [[DNS]]
	- SNMP
	- CHARGEN

## DNS amplification attack
1. Send a [[DNS]] request to a DNS servers with a spoofed source address pointing 
	1. The source address will be the address of the target
2. The DNS servers will respond to the target, overloading it