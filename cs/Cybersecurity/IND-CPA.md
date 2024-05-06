# Overview
IND-CPA is a class of [[Cryptography]] algorithms where:

## Definition
> [!NOTE]
> An algorithm is IND-CPA if it leaks no information about a message $M$ given that:
> - The adversary can observe the communication channel
> - Has access to the encryption algorithm
> 
> In other words, an attacker shouldn't be able to decrypt messages. This is referred to as IND-CPA security

# Drawback
- IND-CPA algorithms can only defend against [[Adversaries#Passive|passive attackers]]
	- E.G people listening to packets
- [[Adversaries#Active|Active attackers]] can change the message without being detected