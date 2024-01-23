---
tags: AccessControl 
---
# Overview
Access control aims to manage access to system resources

# Process
- [[Authentication]]: verify the credentials of the user
- Authorisation: granting permission to access the resource
- Audit: being able to independently review the system records and activities to:
	- Check that the controls are good
	- Check whether the system complies with established policies and procedures
	- Detect security breaches
	- Recommend changes to the control, policy, or procedures

# Access control mechanism

# Definitions
## Object
- A resource
	- Records
	- Files
	- Communication ports
	- Network nodes

## Subject
- An entity that can access objects

## Access right
- The permissions the subject has such as:
	- Reading
	- Writing
	- Executing
	- Deleting 
	- Creating 
	- Searching

# Policies
A policy is the framework that describes what types of access are allowed in each circumstance and who can access them

- [[Discretionary Access Control]]
- [[Role-based Access Control]]
- [[Attribute-based Access Control]]

## Comparison
| Aspect | DAC (Discretionary Access Control) | RBAC (Role-Based Access Control) | ACAC (Attribute-Based Access Control) |
| ---- | ---- | ---- | ---- |
| **Flexibility** | High | Low | High |
| **Granularity of Control** | Fine | Coarse | Fine |
| **Ease of Administration** | Easy | Moderate | Moderate |
| **Scalability** | Bad | Good | Good |
| **Security Maintenance** | Hard | Moderate | Hard |
| **User Ownership of Objects** | Yes | Less | Yes |
| **Role Management** | N/A | Roles | Uses attributes and policies for roles |
| **Access Changes** | Easy | Medium | Easy |
| **Resource Accessibility Control** | Owner | Roles | Attributes and policies |
| **Complexity** | Simple | Moderate | Can be complex |
| **Auditability** | Limited | Good | Good |
| **Performance** | Small-scale only | Role explosion when attributes are added | Difficult to get right, but can be faster at scale |
