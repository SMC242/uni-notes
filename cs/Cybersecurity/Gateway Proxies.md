# Overview
A class of [[Firewalls|firewall]] that acts as a gatekeeper for the network

# Application-level
- The proxy works on the [[Application Layer|application level]]

![Application-level gateway diagram](https://www.ccexpert.us/scnd-2/images/7936_76_59-proxy-firewall-figure.jpg)

# Process
1. User sends request to gateway using [[TCP]]/[[Internet Protocol|IP]]
2. The gateway asks which host the user wants to access
3. The user responds with a [[Authentication|user ID]] and [[authentication]] information such as a [[JWT]]
4. The gateway contacts the host and relays the [[TCP]] segments between the two endpoints

## Comparison
TODO: 18-19

# Circuit-level
TODO: 20-21