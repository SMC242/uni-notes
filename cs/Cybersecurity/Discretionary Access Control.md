---
aliases:
  - DAC
tags:
  - AccessControl
---
# Overview
A [[Access Control]] policy

Controls access based on:
- The identity of the requester
- Some access rules (states what each type of requester can access)

# Access matrix
![Access matrix](https://people.cs.rutgers.edu/~pxk/419/notes/images/access-delegation.png)

- A way of displaying the permissions of each requester

# Access control list
AKA ACL

![ACL](https://i.ytimg.com/vi/d_SUbj1wK5I/maxresdefault.jpg)

- The access matrix decomposed by column
- Might have a default entry with some rights
	- Best practice: rule of least privelege
- Good for finding out which [[Access Control#Subject|subjects]] have rights to access each resource
- Not good for finding the access rights of a user

# Capability lists
![Capability list vs ACL](https://img1.daumcdn.net/thumb/R800x0/?scode=mtistory2&fname=https%3A%2F%2Ft1.daumcdn.net%2Fcfile%2Ftistory%2F265AE543583E521B10)

- Decomposition of the access matrix by row
- A capability ticket ([[Authentication#Tokens|token]]) grants the holder access to an [[Access Control#Object|object]]
- Each user can have multiple tickets and may be authorised to loan them
- The integrity of tickets must be protected (unforgeable)
	- An operating system holds all tickets in an inaccessible region
	- An unforgeable token (such as a random password or cryptographic authentication code) is included in each capability
- Good for finding all the rights a user has
- Bad for getting the list of user with specific access rights for a specific resource

# Protection states
 A snapshot of all the permissions at a point in time

Requirements:
1. Represent the protection state
2. Enforce access rights
3. Allow subjects to change the state in specified ways

## Representation
Adds the following items to the list of objects in the access control matrix:
- Processes
	- Rights to delete, stop, or wake up processes
- Devices
	- Ability to read/write, control, block/unblock
- Memory locations/regions
	- Ability to read/write to certain regions in the memory
	- Default should be no access
	- Subjects
		- Ability to grant/remove access rights

Extra subjects:
- Owners
- Controls
- Subjects with the ability to copy permissions are marked with $*$

### Notation
- Subject: $S_{n}$
- Request type: $\alpha$
- Object: $X$

### Process
1. A subject $S_{0}$ sends request $\alpha$ for object $X$
2. The controller for $X$ checks the access control matrix $A$ to see if $\alpha$ is in $A[S_{0}, X]$]
	1. If access is denied, a warning is raised and action may be taken

# Protection domains
- A set of [[Access Control#Object|objects]] and access rights for them
- Each row of the access matrix specifies a protection domain
	- Each user has a protection domain
	- Processes spawned by the user have the same access rights of the access domain
	- This means that users can spawn processes with a subset of their access rights
	- Defines a new protection domain
