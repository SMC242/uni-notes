# Model
- Can be peer-to-peer or client-server, but is usually the latter
- Used to have persistent communication between clients

# C API
This is the standard C API

- Host `bind`s to a particular [[TCP#Format|IP and port]]
- Host `listen`s for new connections
- When a connection is requested via `connect`, the host may `accept` it
- Once a connection is made, data can be sent with `send` and received with `recv`

## Blocking
- `connect` and `accept` represent the [[TCP#Handshake|TCP handshake]]
- `send` blocks until the data has been written
	- Returns the number of bytes sent (might differ from the input)
- `recv` reads up to `n` bytes
	- Blocks until some data is received or the connection closes
	- Not null-terminated
		- Security risk
- Errors raise exceptions

## `send`
- `send` queues the data for transmission
- It needs to be split into segments first
	- One `send` might be split into multiple segments, or multiple calls might be put into one segment
- Each segment is framed as a [[TCP#Format|TCP packet]]
- The [[Transport Layer#Congestion control|congestion control algorithm]] decides when to allow the packet through the network

## `recv`
- 1 `recv` $\neq$ 1 `send`
- The amount of data returned by `recv` is unpredictable

# Ports
0 - 1023 are for OS services
1024 - 49151 are for user applications
49152 65535 are dynamic ("ephemeral") ports. Used for privacy and peer-to-peer apps

- The IANA has a [registry of ports ](http://www.iana.org/assignments/port-numbers)
- 75% of ports have been registered - there are not enough ports
- TCP clients usually use random ports in the dynamic range