# Overview
Cyber-security is the study of vulnerabilities in software and preventing them

> [!INFO] Trade-offs
> Every decision in cybersecurity is a trade-off. Increasing confidentiality comes at the cost of availability (E.G by using stricter [[Access Control]]). You need to find a balance where the system is secure but still usable
> 
> <img src="https://www.itgovernance.co.uk/blog/wp-content/uploads/2023/02/image-2.png" alt="CIA triad" height="300px" />
> 
><p><small>Image: the CIA triad model</small></p>

# Cryptography
Scrambling data to prevent unauthorised users from reading data

Map of content: [[Cryptography]]

See also:
- [[TLS]]: a protocol for secured communication over [[HTTP]]

# Managing access

## Authentication
#Authentication 

Verifying that a user is who they say they are and preventing unauthorised access

- [[Authentication]]
	- [[Passwords]]

### Authentication protocols
Protocols for authentication

- [[Kerberos]]

## Access control
#AccessControl 

Ensuring that only authorised users can access resources

- [[Access Control]]
- [[UNIX File Access Control]]

### Policies
Strategies for managing access

- [[Discretionary Access Control]]
- [[Role-based Access Control]]
- [[Attribute-based Access Control]]

# Attacks
Methods that [[Adversaries|malicious actors]] can attack systems

- [[Denial of Service]]: depleting a server's resources so that it can't serve legitimate users
- [[Man-in-the-middle]]

##  Defence
Methods for defending against attacks

- [[Firewall]]: controlling who can access a network and how
- [[Intrusion Detection System]]: detecting attacks in progress
	- [[Host-based IDS]]
	- [[Network-based IDS]]

# Tooling

## Offensive
- [[Kali Linux]]
- [[nmap]]

## Defensive

- [[Static Application Security Testing]]
- [[Dynamic Application Security Testing]]