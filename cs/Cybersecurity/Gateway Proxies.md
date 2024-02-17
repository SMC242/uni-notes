# Overview
A class of [[Firewall|firewall]] that acts as a gatekeeper and intermediary for the network. Sits in between the outside world and inside, filtering and forwarding network traffic

# Application-level
- The proxy works on the [[Application Layer|application level]]

![Application-level gateway diagram](https://www.ccexpert.us/scnd-2/images/7936_76_59-proxy-firewall-figure.jpg)

# Process
1. User sends request to gateway using [[TCP]]/[[Internet Protocol|IP]]
2. The gateway asks which host the user wants to access
3. The user responds with a [[Authentication|user ID]] and [[authentication]] information such as a [[JWT]]
4. The gateway contacts the host and relays the [[TCP]] segments between the two endpoints

## Comparison
Pros:
- More secure than packet filters
	- Focused on a limited number of allow-listed applications
- Filters based on content

Cons:
- Overhead for each connection
	- Has to forward traffic in both directions

# Circuit-level
A gateway on the [[Transport Layer]]

- Uses two [[TCP]] connections:
	- Outer host <--> proxy <--> inner host
- Accepts/denies connections based on:
	- The inner host's request
	- Source/destination [[Internet Protocol#IP address|IP addresses]]
	- Source/destination ports

## Comparison
Pros:
- Easy to set up
- Relatively cheap
- Little performance impact

Cons:
- Don't monitor application layer
- Don't inspect packet *contents*