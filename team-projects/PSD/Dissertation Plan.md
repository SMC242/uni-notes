# Points to touch upon

- Why the system was designed the way it was
	- Include systems design
	- https://excalidraw.com/#json=fWH6byb9i9K_6Bsr5vFf5,kg1ywbI_SJO3E9fVfSnRIw
- Features achieved
	- Sign-up/log-in
		- Using NextAuth
	- Paper parsing
		- Using CrossRef's database
		- Reflect on the challenges of parsing academic papers
		- Mention GROBID
	- PDF viewer + screening view
		- Customisable context menu implemented at customer's request
	- Exporting reviews
		- Not integrated into the production branch due to build complexities
		- We were unsure of how to efficiently build this with Docker and an NPM workspace
			- NPM workspaces are a tool for monorepos that manages installing dependencies for all packages
			- Problem: you need the lockfile to install 
			- Problem: how to build only the relevant packages?
			- Solution: don't. Copy all packages, install all dependencies, build only the relevant packages
			- Problem: copying all source code at once would effectively disable caching due to Docker's layer-based caching
- Features not achieved
	- Database and data-access layer
		- We chose to migrate from Prisma to raw SQL 
		- The migration took too long because we spent too much time on safety
			- We wrote a type-safe schema validation library
			- We actually just needed a data-access layer
			- Useful, but developed at the wrong time. Should have been classified as a "would like to have" feature
		- Blocked role-based authentication, creating/updating reviews, voting, migration of authentication
		- The app displays fake data for proof of concept
	- Conflict resolution
		- Dropped due to time constraints
		- Customer provided with a walkthrough of how it could be implemented (created at customer's request)
	- Audit logging
		- Not finished in time
	- Leveraging Next's SSR support for performance
		- The authentication was designed to run on the client only
		- This forced most pages to use CSR
		- Fixable, but we had bigger fish to fry
- How we designed the software to allow future features
	- The customer wanted us to really think about how he can  extend features or add new ones
	- Emphasis on designing the relevant (code) interfaces for each library before coding
	- Showed the customer the interface designs before implementing them

# Points

1. Architectural decisions
	- Monolithic design used
		- All services coupled with database
	- We spoke about a microservice-based design but opted against it for two reasons:
		1. The main place the software will run is on an organisation's server (multi-tenancy implied)
			1. Ability for distributed deployments would not be used in this situation
			2. Gradual upgrading not very useful if customers control when they update the software (unlikely to update frequently)
		2. Our team has little experience with microservices
			1. Too much risk of unforseen problems
	- Serverless also considered, ruled out
		- Many target companies have strict network policies and prefer apps that stay within the local network
	- Microservices would be better for a global instance, but that is not the main deployment situation
3. Overview of features achieved
	- List features achieved
		- Sign-up/log-in
		- File upload + paper parsing
		- PDF viewer + screening view
		- Exporting reviews
			- Note that it wasn't integrated due to build complexities brought with inter-package dependencies and central lockfiles
	- Show final systems design
1. File upload feature
	1. Using CrossRef
	2. Explain complexities of parsing academic papers
	3. Message queue used to allow multiple systems to subscribe to new papers
2. PDF viewer + screening view
	- Composable primitives favoured over complete solutions in the PDF viewer and screening components
		- There will be many variants of these components in the future. Don't try to predict all possible usages
		- Reference: composable primitives over complete solutions
3. Overview of features not implemented
	- Data-access layer
		- Blocked a multitude of features: role-based authentication, creating/updating reviews, voting, migration of authentication
	- Audit logging
		- Ran out of time
	- Conflict resolution
		- Dropped because it was the easiest must-have feature to implement
		- We focused on the harder parts of the project
	- Leveraging Next's SSR
		- Authentication was implemented on the client
		- Most performance on most pages impacted
		- We didn't have time to fix this
4. Why the data access layer wasn't finished and its impact
	1. Decided to migrate from Prisma to raw SQL due to performance limitations in Prisma for our schema
	2. Our priorities were wrong + got distracted
		1. Focused on absolute safety instead of expending minimal effort to implement the features
			1. Created a compile-time schema validation library
			2. This was not what we needed; We needed a data access layer ASAP
	3. This resulted in most areas of the app using mock data because they couldn't connect to the database
5. Designing for the future
	1. Customer stressed the importance of creating an extensible system
	2. Emphasised designing our public interfaces before implementing them
	3. Feedback requested from the customer before moving ahead to coding