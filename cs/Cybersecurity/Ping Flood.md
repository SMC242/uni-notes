# Overview
- A type of [[Denial of Service]] attack
- The attacker floods the server with ICMP echo requests
	- E.G via `ping IP`
- May use [[Denial of Service#IP address spoofing|spoofed source addresses]]

Downsides:
- The attacker's [[Internet Protocol#IP address|IP]] is revealed and can easily be blocked
- The attacker will be flooded with responses

See also: [[SYN Spoofing]]

# Counteracting
- Block the source address
- Block ICMP packets
	- Don't block *destination unreachable* and *time exceeded* packets because those are required for [[TCP]] over [[Internet Protocol|IP]]