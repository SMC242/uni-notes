# All permissions
## Universal
- Manage role permissions
- Assign roles
	- Can only assign a role lesser or equal to yours in the hierarchy (Discord-like)
- Remove users from instance
	- I.E kicking (used in cases of authorised access to your instance or when someone has left the company)

## Team
- Add/remove team members
- Manage team member roles
- Edit team details
	- Name
- Create a new review
- Delete reviews
	- Separate from creating because this is a very destructive action

## Review
Assumption: being able to manage other user implies being able to see their name in the relevant views (and only in those views)

- Add/remove users from a review
- Add a team to a review
- Manage user permissions
- Manage review settings
	- Includes:
		- Name
		- Stage configuration (minimum votes, stages)
		- Rule configuration (rejection reasons, conflict resolution strategy)
		- Data extraction configuration (manage fields)
	- Could be broken into smaller roles for more flexibility but would make role configuration more overwhelming for novices
		- Possible solution: make the section for this permission expandable and allow management of its sub-permissions
		- See figures 1.1 and 1.2
- Add papers to review
- View dashboard
	- Analytics, progress, data extraction info

### Manage review settings design

TODO: convert these to Figma models

![[permission-wireframe.excalidraw]]
Figure 1.1

![[permission-wireframe-2.excalidraw]]
Figure 1.2

## Reviewing
- Vote on a paper
- Extract data from a paper
- Resolve conflicts
- View confidential papers

# Example roles
## Owner
The owner(s) of the instance

- All permissions

## Administrator
People who can manage most things within the organisation, but may have less roles than the owner

## Team manager
Users that can manage members of their team

- Add/remove team members
- 

# Default roles
Roles that would be automatically be created when:

| Event            	| Roles                              	|
|------------------	|------------------------------------	|
| Instance created 	| Owner, Administrator               	|
| Team created     	| Team Manager, Team Member          	|
| Review created   	| Review Manager, Reviewer, Observer 	|