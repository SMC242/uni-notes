---
tags:
  - Docker
  - Dissertation
---
# Overview
This is a list of [[Dynamic Application Security Testing|DAST]] that could be used for [[Docker]]

# silascutler's toolchain
https://github.com/silascutler/awesome-docker-malware-analysis

- An open-source toolchain developed by silascutler on GitHub
- It looks very manual. I'd need to make my own pipeline from it and other tools

# Checkmarx DAST
https://checkmarx.com/checkmarx-dast/

- Paid only and aimed at enterprise
- Not [[Docker]]-specific

# w3af
https://github.com/andresriancho/w3af

- Not [[Docker]]-specific
- Focused on web applications

# ZAP
https://github.com/zaproxy/zaproxy

- An open-source app by Checkmarx
- Multi-platform: desktop, GitHub Actions, Docker container, CLI, web UI
- Not [[Docker]]-specific

# sslyze
https://github.com/nabla-c0d3/sslyze

- Focused on SSL and [[TLS]] configuration vulnerabilities
- Not [[Docker]]-specific

# nmap
https://github.com/nmap/nmap

- A well-known tool for scanning network configuration
	- Listing open ports
	- Getting the OS of the container (for use in automation)
- Not [[Docker]]-specific

# Wireshark
https://github.com/wireshark/wireshark

- A well-known packet sniffer
	-  Could be used to see the communications coming in and out of a container
- Not [[Docker]]-specific

# To look into
- https://github.com/deepfence/ThreatMapper 
- https://github.com/projectdiscovery/nuclei
- https://github.com/greenbone/openvas-scanner
- https://github.com/purpleteam-labs/purpleteam

# See also
-  https://github.com/zhenyaderyaka/dast/: This list was based on this GitHub repo