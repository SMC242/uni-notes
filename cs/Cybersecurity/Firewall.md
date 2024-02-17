# Overview
A system that sits on the boundary between an internal network and an external network. It monitors and controls in/outbound traffic

Objectives:
- All traffic must pass through the firewall
	- It should only be possible to access the network via the firewall
- Only authorised traffic should be allowed through
	- Defined by local policy
- The firewall should be immune to attacks

# Use case
- Prevent unauthorised access
- Prevent vulnerable services from entering/leaving the network
- Protect against [[Denial of Service#IP address spoofing|IP spoofing]] and routing attacks
- Provides a central place for logging
- Provides a location for non-security related functions
	- [[NAT]]
	- Network management (logging internet usage)

## Limitations
- Some attacks bypass the firewall
	- Internal [[LAN]]s can make direction connections to other organisations
- Can't protect against insiders
- Ineffective against [[Social Engineering]]
- A compromised laptop or USB could be infected outside the network and brought inside the network

## Comparison
Pros:
- Simple
- Fast
- Transparent to end-users
- Cheap

Cons:
- Can't prevent application-specific vulnerabilities
	- E.G can't block specific commands
- Vulnerable to vulnerabilities in [[TCP]]/[[Internet Protocol|IP]]
	- E.G [[IP address spoofing]]

# Filters
- Filters are applied to all packets (I.E at the [[Transport Layer]])

These characteristics can be used to filter traffic:
- [[Internet Protocol#IP address|IP address]] and protocol values
	- Source/destination address
	- Port numbers
	- Direction of flow (in/outbound)
- [[Application Layer|Application protocol]] : only allow certain protocols
	- E.G HTTPS must be used unless requesting an allowlisted site
- [[Authentication|User identity]]
- Network activity
	- Time of request (E.G only allow in business hours)
	- Rate of requests (to detect scanning)

## Filtering types
- Source IP address
- Destination IP address
- Source-destination [[Transport Layer|transport-level]] address (port number in [[TCP]] and [[UDP]])
- IP protocol field (allowed transport layer protocol)

## Policies
There are two policies for handling packets:
- Discard: packets are dropped unless they are permitted
	- More conservative
	- Services have to be added on a case-by-case basis
	- Good for high-security applications (banks, businesses, governments)
- Forward: allow unless prohibited
	- Easier for end users
	- Lesser security
		- The admin has to react to threats on the fly

## Filter rules
![Policy table](https://blogs.getcertifiedgetahead.com/wp-content/uploads/2014/07/Table-3.31.jpg)

- Often, source ports are also specified to ensure that the source is using the correct protocol
	- I.E not using HTTP when the rule expects [[SMTP]]
		- Example vulnerability: `dest port > 1023` would allow connections to web servers on port 8080
- [[TCP#Handshake|ACK]] flags can be added to ensure that the ACK flag has to be 1
	- This disallows connections established from outside
	- Connections from outside the network will initially have `ACK = 0`
	- Only allows packets from inside the network because the external service would be responding with `ACK = 1`
	- Vulnerability: the attacker can just set `ACK = 1` in their packets

## Stateful inspection
- Maintain a table about the states of *outgoing* connections
	- [[TCP]] streams, [[UDP]] datagrams, ICMP messages
- When a packet is received, check it against the state table
- Only allow packets that are part of an existing connection

> [!EXAMPLE]
>This table only allows high-numbered source ports when the connection has already been established
>
> ![State table](https://img.brainkart.com/imagebk9/1TsRAvr.jpg)


Pros:
- More granular control
- Can apply rules based on the context (data within the packets)

Cons:
- Less performant
- More expensive than stateless filtering

## Gateways
![[Gateway Proxies#Overview]]

Two main types:
- [[Gateway Proxies#Application-level|Application level]]
- [[Gateway Proxies#Circuit-level|Circuit level]]

> [!NOTE] Comparison
> Comparison for application-level proxies:
> ![[Gateway Proxies#Application-level#Comparison]]
> 
> Comparison for circuit-level proxies:
> ![[Gateway Proxies#Circuit-level#Comparison]]

# Position
A firewall can be in a few positions:
- Bastion host: a system identified by the admin as a critical point in the network
- Host-based firewall: a module installed in each host
	- Available in most operating systems
- Network device firewall: installed in [[Routing|routers]] and switches

## Layering
Multiple layers of firewalls can be used to provide more protection

- An external firewall is placed at the boundary of a network
	- In the boundary router
	- Provides a basic level of protection for the whole network
- An internal firewall is placed between the external and internal network
	- This firewall will be more strict
	- Protects against the DMZ as well as the outside world
	- There can be multiple internal networks, each protected from each other by their own internal firewalls. This creates some isolation
- This leaves a space in between called the demilitarised zone (DMZ)
	- Externally accessible systems are placed here (web, mail, [[DNS]] servers, public APIs)
	- This provides some protection without the stricter rules of the internal firewall
- [[LAN]] switches route traffic into the networks