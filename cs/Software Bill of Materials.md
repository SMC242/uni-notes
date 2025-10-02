---
tags:
  - Cybersecurity
aliases:
  - SBOM
---
# Overview
A list of all software contained in a deployment of software
- This can include the packages installed on the operating system
- Used to audit a system to check for vulnerabilities
- Some modern package managers have a feature for automatically searching the SBOM for vulnerabilities
		- [`npm audit`](https://docs.npmjs.com/cli/v9/commands/npm-audit)
		- [`dotnet list package --vulnerabilities`](https://learn.microsoft.com/en-us/nuget/concepts/auditing-packages)