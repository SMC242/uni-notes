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