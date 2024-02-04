# Overview
A type of [[Denial of Service]] attack using a vulnerability in [[TCP]]. Uses [[Denial of Service#IP address spoofing|forged source addresses]]

# Description
![[TCP#Handshake]]

1. The attacker sends a SYN to the server with a spoofed source address
2. The server responds with a SYN-ACK and one of two things will happen:
	1. The address corresponds to a real service and it will cancel the connection request
	2. The address doesn't exist and the service will repeatedly resend the SYN-ACK packet (until failure is assumed)
3. The transmission control block of the service fills up, causing new requests to be rejected

> [!NOTE] Request volume
> Since the attacker only needs to fill up the transmission control block, the attacker just needs to initially overload it and keep it overloaded. Therefore, the volume can be surprisingly low