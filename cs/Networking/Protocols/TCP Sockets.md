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
`send(bytes)`
- `send` queues the data for transmission
- Frames the data in a [[TCP#Format|TCP packet]]
- May not send all of the bytes; returns the number of bytes sent
- The [[Transport Layer#Congestion control|congestion control algorithm]] decides when to allow the packet through the network

### `sendall`
`sendall(bytes)`
- Repeatedly calls `send` until all bytes have been sent
- It needs to be split into segments first
	- One `send` might be split into multiple segments, or multiple calls might be put into one segment

## `recv`
`recv(n_bytes)`
- 1 `recv` $\neq$ 1 `send`
- The amount of data returned by `recv` is unpredictable

# Ports
0 - 1023 are for OS services
1024 - 49151 are for user applications
49152 65535 are dynamic ("ephemeral") ports. Used for privacy and peer-to-peer apps

- The IANA has a [registry of ports ](http://www.iana.org/assignments/port-numbers)
- 75% of ports have been registered - there are not enough ports
- TCP clients usually use random ports in the dynamic range

# Example
An echo server using Python:
```python
# server.py
import sockets

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
	s.bind(("127.0.0.1", 8080))  # localhost:8080
	s.listen()
	conn, address = s.accept()
	with conn:
		while True:
			data = conn.recv(1024)
			if not data:  break
			conn.sendall(data)

# client.py
with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
	s.connect(("127.0.0.1", 8080))
	s.sendall(b"✨ Hello from the other side ✨")
	data = s.recv(1024)
```