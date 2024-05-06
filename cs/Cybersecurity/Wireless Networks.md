# Overview
Wireless networks communicate via radio waves instead of [[Physical Layer|physical links]]

Essential components:
- Wireless client ("endpoint"): a device that uses wireless networking
	- Smartphones
	- WiFi devices
	- Bluetooth devices
	- Wireless sensors (infrared)
- Access point (AP): the infrastructure for a wireless network
	- Cell towers
	- WiFi hotspots
	- Wireless APs for local or wide-area networks

See also:
- [[WLAN]]: the protocol for wireless networks

# Security risks
- Usually uses broadcasting
	- Easier to eavesdrop or jam than wired connections
- Mobile endpoints have extra risks
	- Theft/tampering
	- Untrusted content (E.G malicious QR codes, NFC reader scams)
	- The device knows its location
		- An immobile device usually only knows its [[Internet Protocol#IP address|IP address]] which isn't very accurate
		- Risk of doxxing
- Limited resources
	- Vulnerable to [[Denial of Service]] or malware that consumes resources (E.G by filling up the backing storage)
- Devices often left unattended --> may be tampered with
	- Particularly relevant for sensors
- Malicious association: malicious APs
	- Similar idea to [[Phishing]] - pretend to be legitimate and steal [[Authentication|credentials]]
- Ad-hoc networks (peer-to-peer) have no central access point
	- So you can't put a central security layer in the middle like an [[Intrusion Detection System]]
- Identity theft
	- You can spoof [[MAC Addresses]] to impersonate other devices
- [[Man-in-the-middle]] attacks
- [[Denial of Service]] attacks
- Network Injection attacks: adding fake packets into a connection managed by an AP
	- APs that don't filter network traffic are vulnerable to this

## Wired networks
- Wired networks are more secure because the attacker has to be physically connected to the network
	- E.G they sneak inside the building and find an Ethernet cable
- This is a lot harder than just standing outside and connecting

# Security measures
- [[Cryptography|Encrypt]] the transmissions
- Disable SSID (service set identifier) broadcasting through APs
- Require [[Authentication]] when endpoints connect to the network
- Only let computers that know the identity of the routers connect to the network
- Always change the administrator password
- Use firewalls and antivirus software

