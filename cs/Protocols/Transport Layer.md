---
tags:
  - OSIModel
---

# Purposes
- Hide the [[Network Layer]]
- Make the network seem reliable (it's not)
- Improve the quality of service from the [[Network Layer]]
- Make interfacing with the [[network layer]] easy
	- E.G programmers working on the [[Application Layer]] might work with the sockets API
	- Operating systems usually handle the [[Network Layer]] stuff

## Roles
- Process-to-process rather than host-to-host
- Multiplexing
- Reliability
- Framing
	- Some protocols have a defined structure
- Congestion control

# Multiplexing
- A [[Network Layer]] address identifies a host, while a transport layer address identifies a user process (service) running on a host
- This means that multiple transport services can be running on one host

# Reliability
- Since the [[Internet Protocol]] is best-effort, [[packets]] may be dropped
- This is fine for some use cases, but not others
	- Email needs 0% data loss
	- Voice and video streaming can handle some loss but requires speed

# Congestion/flow control
- This layer manages the *application sending rate*
- It adjusts the sending rate depending on how fast the [[Network Layer]] is able to deliver the data and how fast the receiver is processing it
- Done end-to-end because the endpoints know the path taken and how long it took

Flow control = adjusting for the receiver
Congestion control = adjusting for the [[network layer]]

## Elasticity
<dl>
	<dt>Elastic</dt>
	<dd>An application that doesn't care about send rate - speed is <i>nice to have</i> but not required</dd><dt>Inelastic</dt>
	<dd>An application that cares about send rate - there are minimum and maximum send rates</dd>
</dl>
# End-to-end principle
- Avoid centralising functions in networks as much as possible
- Instead, systems on both ends should handle things

This is because the network is unreliable, so validation should happen at each end

# Transport protocols list
Many protocols use ports as a service identifier. This allows multiple services to run on the same host

Protocols with a D in their name use "datagrams", which basically means that they're framed

## Core
- [[UDP]]
- [[TCP]]

## Extra
Not as important to know about as these are up-and-coming protocols. 
- [[DCCP]]
- [[SCTP]]
- [[QUIC]]

It's difficult to deploy new protocols because firewalls look at packet headers and payloads and block anything they don't understand. This means that firewalls must add support for new protocols

In order to use new protocols, applications "tunnel" by going through the core protocols
- WebRTC -> [[SCTP]] -> DTLS -> UDP
- QUIC -> [[UDP]]

## Sockets
Two types:
- Stream: [[Signals#Virtual circuits|virtual circuit service]]
	- Uses TCP/IP
- Datagram (framed): packet service
	- Uses UDP/IP

# Congestion control
There are pros and cons of implementing this on the [[Network Layer]] vs the [[Transport Layer]]

### Network layer
<ul class="breakdown">
	<li class="pro">Safe</li>
	<li class="pro">All transport protocols will be congestion-controlled</li>
	<li class="con">Forces all applications to use the same algorithm</li>
</ul>

### Transport layer
<ul class="breakdown">
	<li class="pro">Flexible</li>
	<li class="pro">Can optimise for application needs</li>
	<li class="con">Bugs can congest the network</li>
</ul>
