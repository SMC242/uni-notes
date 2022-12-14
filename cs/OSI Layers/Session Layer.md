# Roles
- Find participants in a session
	- Look up names in a directory like [[DNS]]
	- Servers might direct a connection to a peer (instant messenger, VoIP call)
- Handles setting up connections
	- If you try to directly connect to a named host, [[NAT]] will cause issues
	- The server handles discovering services to direct peers to
	- The peers then connect peer-to-peer
- The group size may change
	- E.G people joining and leaving a VoIP call
- Handles mobility
	- Mobile devices are reassigned [[Internet Protocol#IP address|IP addresses]] as they move
	- The session layer must handle this
	- The old location must redirect to the new location
- Multiple [[Transport Layer]] connections might be used in a single session
	- A client might make a request for web page and the images contained within
	- The web page may also make REST requests to other APIs

# Strategies
## Caching
- Frequently used content is held in memory for faster access
	- Can be done server-side to reduce response time or client-side to prevent duplicate requests

## Middleboxes
- An email server holds mail until the user checks their emails ("disconnected operation")
- Instant messaging servers do the same
- Should only be used when necessary
