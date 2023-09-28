---
tags: OSIModel

---
- The layers above [[Transport Layer]] are considered the "higher layers" because they're what back-end devs spend most of their time dealing with
	- The application or its dependencies usually implement this stuff rather than the OS kernel
- The boundaries between these layers are loose
	- Can be treated as one layer because layering isn't required at this level

# Roles
- Manage [[Transport Layer]] connections
- Name and find resources within the app
- Transform and communicate about data formats (E.G JSON, XML)
- Present data in a way that suits the needs of the app
- Enforce application-level rules/semantics

![[Session Layer]]

![[Presentation Layer]]

![[Application Layer]]]