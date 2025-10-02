---
tags:
  - Docker
  - Dissertation
  - SAST
  - Cybersecurity
---
# Overview
This is a list of [[Static Application Security Testing|SAST]] for [[Docker]]
# Trivy
https://github.com/aquasecurity/trivy

- Free
- Simple to use
# Grype
https://github.com/anchore/grype

- Free
- Flexible output format
## Syft
https://github.com/anchore/syft

- A tool developed by the same company for generating a [[Software Bill of Materials]]
- Might come in handy
# Dockle
https://github.com/goodwithtech/dockle

- A linter with some security features
- Not particularly focused on vulnerabilities beyond the basics like "Don't store passwords in the `Dockerfile`"

# OSV Scanner
https://github.com/google/osv-scanner?tab=readme-ov-file#container-scanning

- A vulnerability scanner that has support for scanning [[Docker]] containers
- Made by Google
- Support for many languages
# Dive
https://github.com/wagoodman/dive

- A tool for analysing layers of [[Docker]] containers

# HarborGuard
https://github.com/HarborGuard/HarborGuard

- An open source tool that integrates the following scanners
	- [#Trivy]
	- [#Grype]
	- [#Syft]
	- [#Dockle]
	- [[#OSV Scanner]]
	- [#Dive]