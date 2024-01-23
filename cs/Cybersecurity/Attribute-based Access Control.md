# Overview
An [[Access Control]] policy based on attributes of the user

# Attributes
There are 3 categories of attributes:
- [[Access Control#Subject|Subject]]
	- Name
	- ID
	- Organisation
	- Job
	- Role
- [[Access Control#Object|Object]]: file metadata
	- Title
	- Subject
	- Date
	- Author
- Environment: the environment that the access is occurring in
	- Current date/time
	- Network security level

# Policy
- The set of rules and relationships within an organisation
- A [[Access Control#Subject|subject]]'s privileges are embodied in a policy
- Policies are written from the perspective of the [[Access Control#Object|object]] and the privileges available to subjects

## Notation
- $s$: subject
- $o$: object
- $e$: environment
- $SA_{n}$: subject attribute
- $OA_{n}$: object attribute
- $EA_{n}$: environment attribute

>[!NOTE] Attribute relations
> Here is an example for a subject $ATTR(s) \subseteq SA_{1} \times SA_{2} \times \dots \times SA_{k}$

### Policy rule
- Some boolean function parameterised by the subject, object, and environment's attributes

$$allowed = f(ATTR(s), ATTR(o), ATTR(e))$$
## Policy store
- A set of policy rules

> [!EXAMPLE]
> An example policy store for movies:
>
| Age Rating | Users Allowed |
| ---------- | ------------- |
| R          | Age 17+       |
| PG-13      | Age 13+       |
| G          | Everyone      |
>
>Policy rule:
>$$
>\begin{align*}
>R_{1} &= Age(u) \ge 17\\
&\lor (Age(u) 13 \land Age(U) \le 17 \land Rating(m) \in \{PG, G\})\\
&\lor (Age(u) \le 13 \land Rating(m) \in \{G\})
\end{align*}
>$$

> [!EXAMPLE] Adding subscription levels
> If you wanted to allow users with a premium subscription to access new films and limit regular users to old films, the policy rule would be:
> 
> $$\begin{align*}
> R_{2} &=  (Membership(u) = Premium) \\
> &\lor (Membership(u) = Regular \land MovieType(m) = Old)
\end{align*}$$
>
>A third policy rule would be required to join both rules together:
>$$R_{3} = R_{1} \land R_{2}$$