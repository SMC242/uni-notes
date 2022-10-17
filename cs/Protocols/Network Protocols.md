<dl>
<dt>Protocol</dt>
<dd>A mutually agreed way to communicate between multiple hosts; a communication standard</dd>
<dt>Protocol data unit (PDU)</dt>
<dd>A type of message - structured in a standardised way</dd>
</dl>

# Characteristics
- A syntax to serialise the data (E.G JSON)
- When and how messages can be sent

# PDUs
Two main types:
- Textual
- Binary

Textual PDUs have a syntax and grammar
- HTTP/1.1, SMTP, SIP, Jabbe

Binary PDUs only have rules for the format like big/little endian or 32/64 bit
- TCP/IP, RTP

# Constraints
PDUs can be thought of as the laws of the messages within a protocol
- Who can send them?
- Is the message valid?
- When can they be sent?
- Roles for the hosts (client and server or peer to peer)
- How to handle errors

# Protocol standards
- A formal spec for a protocol
- Ensures interoperability

# Further reading
[[Protocol Layering]]
