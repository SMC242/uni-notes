# All permissions

Roles would be created within a context (E.G an organisation, team, or review) and would only grant permissions within that context. For example, a user granted *add papers to review* in the "Patent Law and Artificial Intelligence" review would not have that permission in the "Infective Endocarditis" review.

## Assumption:    
Having role in a team or review implies that you can see the review. 

Example: Jesus from Datasky is granted Reviewer in the "Wikimedia Imagery Value" review. He can now see the review and explore its non-confidential files.

## Universal

- Manage role permissions
- Assign roles
	- Can only assign a role lesser or equal to yours in the hierarchy (Discord-like)
- Remove users from instance
	- I.E kicking (used in cases of authorised access to your instance or when someone has left the company)
- Edit organisation details
	- Name
- View organisation members

## Team

- Add/remove team members
- Manage team member roles
- Edit team details
	- Name
- Create a new review
- Delete reviews
	- Separate from creating because this is a very destructive action

## Review

#### Assumption
Being able to manage other user implies being able to see their name in the relevant views (and only in those views)

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

Here are two sketches for how the nested permissions could be presented to the user

![Sketch 2](https://i.ibb.co/pnV37BG/permission-wireframe-excalidraw.png)
Figure 1.1

![Sketch 2](https://i.ibb.co/qjnfgY8/permission-wireframe-2-excalidraw-1.png)
Figure 1.2

## Reviewing

- Vote on a paper
- Extract data from a paper
- Resolve conflicts
- View confidential papers

# Default roles

Roles that would be automatically be created when:

| Event            	| Roles                              	|
|------------------	|------------------------------------	|
| Instance created 	| Owner, Administrator               	|
| Team created     	| Team Manager, Team Member          	|
| Review created   	| Review Manager, Reviewer, Trusted Reviewer 	|

## Owner

The owner(s) of the instance

- All permissions

## Administrator

People who can manage most things within the organisation, but may have less roles than the owner. By default, they would have all permissions except:

- Manage organisation details

## Team manager

Users that can manage members of their team. Can't delete reviews by default.

- Add/remove team members
- Manage team member roles
- Edit team details
- Create a new review

## Team member

A member of a team. This role would provide no extra permissions and only exists to provide an obvious place to configure permissions for such users.

## Review manager

A user who can update the review and assign work to [reviewers](#Reviewer).

- Add/remove users from a review
- Add a team to a review
- Manage user permissions
- Manage review settings
- Add papers to review
- View dashboard

## Reviewer

A user who can vote on papers within a review. Can resolve conflicts by default

- Vote on a paper
- Extract data from a paper
- Resolve conflicts

## Trusted Reviewer

A [reviewer](#Reviewer) that can view papers marked as confidential. 

- View confidential papers

# Other example roles

## Observer

A user that can view a review and its progress but cannot interact with it. Could be used for stakeholders. Provides no extra permissions.

## Independent reviewer

A user that can resolve conflicts, but otherwise cannot influence the review. If this role was in use, the review manager would want to remove "resolve conflicts" from the reviewer role.

- Resolve conflicts

