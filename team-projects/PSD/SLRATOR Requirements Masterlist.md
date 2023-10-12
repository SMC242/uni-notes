| Symbol 	| Meaning      	|
|--------	|--------------	|
| !!     	| Must have    	|
| !      	| Should have  	|
| +      	| Nice to have 	|
| X      	| Conflict     	|

# Core functions
- Delete files !
# Uploading
- Batch upload !!
	- From local folder 
	- Should be extendable to pull from APIs !
- Support uploading PDFs initially !!
- Should be extendable to use other formats !
	- EndNote XML, PubMed, RIS, Zotero

# Categories
Be able to accept/reject/skip files

Statuses:
- Yes !!
- No !!
	- Conflict !!
		- Goes into queue for later
- Later X
	- Goes into backlog

# Search filtering
- Search the index of the files !
	- Filter by: author, keyword, publication date (year range)
	- Multiple filters can be used at a time
	- These should be buttons - not a query language

# Exporting
- What was accepted, rejected, reviewers !!
- Pick output format !
- Pick referencing style !
- Summarise with LLM +

# Collaboration
- Users should be anonymous *between reviewers !!
- Live collaboration +

## Roles
- Each license has an owner
	- One license may have many projects
	- Each project has some owners and some reviewers
- Roles are not mutually exclusive

### Owner !!
- See names of reviewers
- Manage reviewers
- There must be at least one owner

### Reviewers !!
#### Core reviewer !
- Their votes are required 
- There must be at least 2 core reviewers

#### Conflict resolver !
- Solves conflicts
- Votes are not required for anything but conflicts

#### Spectator +
- Can view everything, but cannot do anything

# Highlighting
- Set include-exclude criteria !
- Configure per stage +
- Use word association ++

# Dashboard !
## Main dashboard !
- Overview of statuses of all files: !
	- Finished
	- One vote
	- Conflict
	- No votes
- Statistics + 
	- Acceptance rate
	- Rejection rate
- Embed productivity management software ++
## Owner dashboard + 

# Notifications
- Emails when: !
	- You're added to a project
	- When you are assigned work
	- There is a conflict you are responsible for
- Team communication software integration: +
	- MS Teams
	- Google Workspaces

# Paper queues !!
- To be reviewed !!
- Reviewed with categories: !
	- Accepted
	- Rejected
- Conflicted !!
	- Separate queue
	- Should not interrupt main workflow
## Conflict resolution !!
- Conflict resolvers are notified !
- They must vote in order for the paper to exit the conflict queue !!
- Conflict resolution strategy should be configurable at the project level:
	- Intermediary strategy
	- Re-voting by core reviewers

# Assign workload +
- Core viewers assign workloads to normal reviewers
- Core reviewer effectively becomes a management role
- Work out the details of this

# PDF Viewer !
- Zoom
- Comment
- Heading outline
- Mini-map +
- Search text
- Highlight relevant words (inclusion/exclusion)
	- Highlighting criteria menu
- Split into relevant sections (title, abstract, full text)
- Vote buttons (accept, reject, skip)

## Security
- Files should be encrypted when they are uploaded to the database !
- Sign in with: !!
	- Google
	- SSO