# Inter-domain routing
Between [[Network Layer#Autonomous systems ASes|autonomous systems]] (ASes)
- Each ISP network is autonomous
- They don't trust each other
- They also use different tech and policies

# Algorithm goals
Goal: find the best route to the destination
Solution: graph algorithms

- Treat each network as a node in a graph (AS topology graph)
	- Routing occurs without considering the internal topology of each network
	- Topology is constantly changing due to the internet growing and shrinking and people changing ISPs
- Treat connections between the ASes as edges
- Edge networks (local, regional ISPs) use a route to the core (tier 1 network)
- Core networks hold a full routing table and must know about every other network
	- These are called default free zones (DFZ)
	- This is achieved with Regional Internet Registries (RIRs) and Internet Exchange Points (IXPs)
	- There's one RIR per continent

## DFZs
- The DFZ is used when there is no default route
- Routes based on policies such as:
	- Politics (some countries block other countries)
	- Some ASes are preferred over others
	- Efficiency (use the least hop between ASes)