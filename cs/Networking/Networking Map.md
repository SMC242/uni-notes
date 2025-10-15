# Overview
Networking is the field of data transfers from one computer to another

# Protocol Layering
In order to manage complexity, layers of abstractions are built on top of each other. These layers come together to support the [[Internet Protocol]]

See: [[Protocol Layering]]

# Protocols
## Fundamental
- [[UDP]]: fast, unreliable datagram protocol
- [[TCP]]: reliable packet protocol
	- [[TCP Sockets]]
- [[Internet Protocol]]: how packets are sent across the internet
- [[WLAN]]: how wireless networks work
	- [[RSN]]: WLAN's successor
- [[NAT]]: translating public/private [[Internet Protocol#IP address|IP addresses]]
- [[DNS]]: resolving [[Internet Protocol#IP address|IP addresses]] from domain names

## Other
- [[ARP]]: converts [[MAC Addresses]] to [[Internet Protocol#IP address|IP addresses]]
- [[TLS]]: encrypting traffic
- [[QUIC]]: a fast, reliable yet connectionless protocol
- [[DCCP]]: like UDP but with congestion control
- [[SCTP]]
- [[SIP]]: voice over [[Internet Protocol|IP]]
- [[SMTP]]: email protocol

# Concepts
- [[Protocol Layering]]
- [[Addressing]]
	- [[MAC Addresses]]
- [[Best Effort Servicing]]
- [[Routing]]
- [[Network Bridge]]
- Error detection with [[CRC]]s