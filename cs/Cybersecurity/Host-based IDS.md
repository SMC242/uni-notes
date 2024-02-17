---
aliases: HIDS
---
# Overview
A type of [[Intrusion Detection System|intrusion detection system]] deployed on individual computers that monitors the events inside the host

- Can detect internal and external intrusions
- May use [[Intrusion Detection System#Analysis|anomaly, signature, or heuristic approaches]]

# Sensors
- System call traces: a record of the sequence of system calls each process uses
- Audit logs
- Integrity [[checksums]]: a [[Hashing|hash]] added to packets on transmission that is verified when received
	- Critical files are scanned for changes

# Analysis
- Anomaly-based:
	- Check how programs access core [[kernel]] functions
	- Verify [[checksums]] on key files
		- Can't detect changes made to processes once they're running (E.G editing memory)
- Signature/heuristic
- Distributed HIDS
	- Used by organisations to defend [[LAN]]s
	- Coordinates IDSes across the network