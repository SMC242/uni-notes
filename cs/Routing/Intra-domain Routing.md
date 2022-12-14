# Intra-domain routing
Within an [[Network Layer#Autonomous systems ASes|autonomous system]] - local, nationwide, or worldwide

- A system trusts obviously trusts itself
	- It knows its own tech, policies, and security
- Ends up being more efficient because it knows its own network topology
- Runs on IP routers
	- Regions within the network topology are represented as IP prefixes

Two methods:
- Distance vector (Routing Information Protocol, RIP)
- Link state (open shortest path first, OSPF)
- Only one protocol is used in each network

# Distance Vector Protocols
- Only used for small networks
- Each node maintains a vector representing the distance to every other node in the network
	- This gets periodically exchanged with neighbours to ensure all nodes have up-to-date information
	- It takes some time to to be fully correct ("converges on a steady state")
	- Offline or unknown links have distance = $\infty$
- At the start, each node only knows the distance to itself
- Then it calculates the distance to all of its neighbours
- Then it receives the routing state from its neighbours and updates its own state (it knows its neighbours' neighbours now)
- If a link fails, the neighbours set the distance vector to $\infty$ and that propagates
	- Leads to updating the routes
- Updates can be outdated
	- Leads to conflicts between updates from neighbours and causes a loop of disagreeing updates
	- This issue is known as <font style="color:red;">counting up to infinity</font>

[Distance vector example](https://youtu.be/hdpnoOcrGck?t=370)

## Counting up to infinity
- This can't be entirely solved
- This is why distance vector algorithms are only used for small networks
### Solution 1
- Define $\infty = 16$
- This puts a cap on how long the system will count to infinity
- Only works if the network is less than 16 hops across

### Solution 2
- When updating, don't send a route back to a neighbour
- Prevents two-node loops but not 3

# Link State Protocols
- Nodes know the links to their neighbours and the distance of using those links
- This information is "flooded" through the graph to provide all nodes with a map
- Each node then calculates the shortest path to every other node and stores that in their routing table
- Instead of periodically refreshing like [[#Distance Vector Protocols]], updates only happen at the start and when the topology changes
- Forwards [[packets]] based on the shortest path
	- This decision is based on weights decided by the routing protocol

## Update format
- The address of the node that sent the update (who sent it)
- The neighbours of the node
	- The distance to each neighbour
	- In the case of a link that connects multiple hosts, it also sends the range of addresses of the link
- A sequence number (think of it as the index of a loop)

## Updating
- When an update is received from a node, the receiver compares the sequence number with the number of the update it last got from that node
	- Only accepts the update if the sequence number increased
- If the update is accepted, the update will be propagated and the routing table will be recalculated
- Does not send the update back to the sender
- This process repeats until all nodes have received the update

Important note: the sequence number is for the original sender, not the neighbour the update was received from

# Protocol comparison
## Distance vector
- Easy to implement
- Routers store only the distance to each node
	- $O(n)$
- Slow convergance
	- Unsuitable for large networks 

## Link state
- Complex to implement
- Routers store the whole network map
	- $O(n^2)$
- Fast convergance
