---
aliases:
  - OSIModel
---

# Protocol layering
Protocols are often built on top of each other. They each only communicate on their layer

- Reduces complexity by breaking the problem of communication into pieces
- The lowest level layer is the one that is closest to the ethernet cables/WiFi

# OSI Reference Model
- The example that protocols are designed from
- It helps to make all the required considerations
![OSI diagram](https://static.studytonight.com/computer-networks/images/complete-osi-model-2.JPG)

# Physical layer
Important that this is as fast as possible because everything sits on top of it

- The properties of the cable or fibre optic used
	- Size, shape
	- Max length
	- Type of cable (voltage, current, modulation)
- Optical fibres are faster due to encoding the data in light which travels fast ($3\times 10 ^8 m/s$)
- The characteristics of the wireless network (WiFi)
	- Radio frequency
	- Transmission power
	- Type of antenna

See [[Physical Layer]]

# Data link layer
How local networks connect hosts together

- The structure of the physical layer's bit stream
- Handles splitting the stream into messages
- Detecting errors in messages
- Assign addresses to hosts 
- Flow control
- Examples: ethernet, 802.11

See [[Data Link Layer]]

# Network layer
- Connects between networks (source host to destination host)
- The first layer that enables end to end connections is the network layer 
- Manages data delivery, naming, addressing, routing, flow control
- E.G [[Protocol List#IP|IP]], ICMP

See [[Network Layer]]

# Transport layer
- Makes data transfer reliable
- Handles ordering and congestion control to avoid dropping packets
- E.G [[Protocol List#TCP|TCP]], [[Protocol List#UDP|UDP]]

# Session layer
- Manages multiple transport layer connections
- E.G NeBIOS, RPC

# Presentation layer
- How the data looks and conversion
- Converts to character sets like ASCII or UTF-8
- Uses data markup languages like XML, HTML, JSON
- Content negotiation (E.G MIME, SDP)
- E.G SSL, Telnet, HTTP/HTML

# Application layer
- How an app communicates
- WhatsApp uses XMPP
- Facebook Mobile uses MQTT
- E.G HTTP/HTTPS, [[Protocol List#DNS|DNS]], [[Protocol List#FTP|FTP]]

![OSI Summary](http://www.tech-faq.com/wp-content/uploads/2009/01/osimodel.png)