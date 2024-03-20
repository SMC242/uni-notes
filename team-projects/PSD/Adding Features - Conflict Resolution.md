# Overview

This document describes how a future developer would go about implementing conflict resolution. I chose this feature because it requires modifications to the database, data layer, and frontend. Additionally, it uses the audit logging system.

## Assumptions

1. The database and data-fetching layer have been completed in a similar way to the code on the `feature/database` branch
2. Audit logging has been implemented by publishing messages to the message queue
	1. The messages follow the formats specified in `packages/auditLogging/src/auditLogging.ts` on the `feature/auditLogging` branch
3. The `/review` route has been converted to a [Next.js dynamic route](https://nextjs.org/docs/app/building-your-application/routing/dynamic-routes) containing the review ID (E.G `/review/[reviewId]`)
4. An API for obtaining the logged-in user's reviewer ID has been implemented

Role-based authentication is outside the scope of this document as nothing substantial was implemented, so we are unable to advise on how its implementation would be used. The basis for creating and updating roles is on `feature/databaseRoles`, but authentication has not been developed.

# Table of contents

The steps required to implement conflict resolution have been grouped by service

1. [Database](#database)
	1. [Data layer](#data layer)
2. [Frontend](#frontend)
	1. [Components](#components)
	2. [Pages](#pages)
3. [Logging](#logging)
4. [Running](#running)

# Database

- The database we are using is [PostgreSQL](https://www.postgresql.org/)
- There is a conflict status flag in the `PaperReview` table called `inConflictState`
	- `PaperReview` contains papers within reviews
- See `packages/database/init.sql` for the schema
	- This schema is loaded when starting the database with `docker compose up` (configured in `packages/docker-compose.yaml)
		- See also: https://docs.docker.com/compose/ 

## Data layer

<aside>The base path used in in this section is <code>packages/database/src</code></aside>

1. Define an update command in `CRUDCommands/UpdateCommands/papersReview.ts`
	- This command should run an SQL `UDPATE` query that updates some fields in a `PaperReview` record
	- It will need to take a reviewer ID as a parameter as well as a `Partial<PaperDatabase>` (`PaperDatabase` is defined in `CRUDCommands/ReadCommnads/papers-reviews.ts`)
	- We will use this command to offer an endpoint for updating papers
3. Define a new `PATCH` endpoint `/review/:reviewID/:paperID` in `api/routes.ts` that wraps the update command created previously
	- This REST API is implemented using [Express.js](https://expressjs.com/)
	- The body will need to contain a reviewer ID
4. Define a read command in `CRUDCommands/ReadCommands/papersReview.ts`
	- It should run an SQL `SELECT` query that finds all papers with `inConflictState = true`
5. Define a `GET` endpoint `/review/:reviewID/in-conflict` in `api/routes.ts` that wraps the read command created previously

We will use the GET and PATCH handlers `/review/:reviewID/:paperID` to update the `inConflictState` flag of a given paper. The `GET` handler for `/review/:reviewID/in-conflict` will be used to find the papers relevant to the conflict view

# Frontend

## Components

<aside>The base path used in in this section is <code>packages/frontend/src/app/_components</code> (unless specified otherwise)</aside>

The `_components` directory contains components that are re-used across the app. Directories in `packages/frontend/src/app` are mapped to a URL -  a page if they contain a `page.tsx` or a set of HTTP handlers if they contain a `route.ts`. Directories will not be mapped to URLs if they are prefixed with an underscore. See also: [Next.js' App Router documentation](https://nextjs.org/docs/app/building-your-application/routing)

### Assumptions

- You have wrapped the PDF viewer component with extraction fields instead of modifying the existing one
	- During conflict resolution, you may not want a reviewer to be able to extract data
	- There are many foreseeable applications of the PDF viewer where extraction fields would not be desirable, such as viewing the list of papers in a review

### Making the conflict viewer component

1. Create a new component in `screening/screening-stage/conflict.tsx`. It should take in an `InternalPaper` (located in `packages/frontend/src/models/InternalPaper.tsx`)
	- This component should wrap `PdfViewer` from `pdf/pdf-viewer.tsx`
		- The `PdfViewer` supports extension in three ways:
			1. The `children` prop
				- This allows you to pass [JSX](https://react.dev/learn/writing-markup-with-jsx) to be rendered inside the PDF viewer
			1. The `contextMenu` prop
				- This allows you to customise the look and functionality of the context menu. See the components in `pdf/context-menu`
			2. Wrapping it with JSX (will be rendered outside of the PDF viewer)
	- This will be used to display a conflicted paper
2. Modify `findStage` in `screening/screening-stage/screening-stage.tsx` to support your new component
3. Refactor `ScreeningStageControllerProps` and `ScreeningStage` to make `stage` a generic type (E.G `<Stages extends string>)
	- We will use this to restrict the stages to just "conflict"
		- `ScreeningView` expects a mapping of review stages to paper IDs. We will create a situation where the mapping is just `{ "conflict": [ "<paper IDs here>" }` (I.E there is only one stage)
	- See [Snippet 1](#snippet 1)
4. Refactor `ScreeningView` in `screening/screening-view.tsx` to allow the `StageControls` (located in `screening/stage/controls.tsx`) to not be displayed
	- Suggestion 1: take in a boolean prop called `stagesEnabled` and use it to [conditionally render](https://react.dev/learn/conditional-rendering) the `StageControls`
	- Suggestion 2: create a [slot](https://www.dhiwise.com/post/mastering-react-slot-patterns-for-flexible-ui-components) for the header
		- This would allow you to customise the header of a `ScreeningView`. This should be used if you anticipate more than just `StageControls`/no `StageControls` in the header
	- Warning: don't forget to modify the error state for an empty stage
		- It currently renders a `StageDropdown` (located in `screening/stage/stage-dropdown.tsx`) which would allow the user to change the stage
5. Refactor `ScreeningViewProps` (located in `screening/screening-view.tsx`)  such that the keys of `papers` are a generic type. See [snippet 1](#snippet 1) for an example
6. Refactor `ScreeningView` to take in the generic type for `ScreeningViewProps`
7. Refactor `ScreeningView` to take in `afterAccept`, `afterReject` callbacks of type `(paperId: string, vote: VoteValue) => Promise<void>`
	- These callbacks are to be called during the `onAccept` and `onReject` callbacks respectively. `on{Accept,Reject}` should be passed to `ScreeningPanel` 
	- These callbacks will do the API calls to update the paper in the database (either to add a vote or to resolve a conflict)
	- The function body prior to the `after{Accept,Reject}` calls should call `dispatch({ type: "increment-index" })` in order to advance the view to the next paper

#### Snippet 1
View this [TypeScript playground](https://www.typescriptlang.org/play?ssl=8&ssc=1&pln=9&pc=56#code/C4TwDgpgBACghpATgSQCZQLxQM7EQSwDsBzAbgChzRIoBlAY0QgkKOIDV8IB3GRAezDYAPLWBxiEbFAgAPYC1TTcBEgD5MUAN7kAkGAQRE2AFyw4iYPjgAbYQCUI9folSjxk7ABpzSNAG0AXTU1Cl0AenCoADpY8gBfCkioAD0Afkpkx2AAV0RWEihgAAtoamh+ADMcDykoSpci0pw4AGsK6rk4AFswGwhySpzCeit+QjpGZgKOLm53CTq5BUIlGtViNQAKMAEhMwYmFjZOHj5BETFF7DUASgPa7CDtcigoJlz8qAB5ACMAKycwGi7RA2B2e2w0QMSGwtygcGkV08QQo8UyUQAKsV8NJylB6HAJr9oERcESrHAFOhuPgSlBUPhKpUjCxgDgIMBpFUoKCoeRkgAJfgANyMUFF4oABuTPABGKUI1ZQGWPABMiuA-A5ZWa5W51RK0D55GchFwNWucs0h2mJzmWx0+kMxjMTt0VmA-TMQS8el0cF+Kjgox9gT9ul0ACIhjYbABaBTyKNhiPxP3xW6UM0W2VSNU2qbHEinbiO15QGFGUwvN5vKNmyo2fCjFNQIIV9GZoA) for an interactive demo 

## Pages

<aside>The base path used in in this section is <code>packages/frontend/src/app</code></aside>

1. Create a `page.tsx` in `review/[reviewId]/conflict`
2. Fetch the papers in conflict using [React Query](https://tanstack.com/query/latest)
	1. Send a `GET` request to `/reviews/:reviewID/in-conflict` (the base URL depends on how the data layer is hosted)
	2. Verify that the response was successful using [Zod](https://zod.dev/)
	- Recommended: do this in a new hook in `_hooks` called `useInConflict`
		- This will allow you to re-use this query across the app
3. Render a `ScreeningView` with the following props:
	- `papers={{ conflict: papers }}`
		- `as const` may be required to narrow the type to `Record<"conflict", InternalPaper[]>`
	- `reviewer={ currentReviewer }` (use the API for getting the current reviewer)
	- `initialStage="conflict"`
	- `afterAccept`: a function that sends a `PATCH` request resolving the conflict and accepting the paper to `/review/:reviewID/:paperID`
	- `afterReject`: a function that sends a `PATCH` request resolving the conflict and rejecting the paper to `/review/:reviewID/:paperID`
4. Check that `/review/[reviewId]/vote` was not broken by the generic type change and fix it if it was

## Logging

<aside>This section was separated from "Data layer" for the sake of focus, but could easily be done concurrently. The base path for this section is <code>packages/</code></aside>

1. Define a new schema in `auditLogging/src/auditLogging.ts` for resolving conflicts. 
2. Add your schema to `auditLoggingSchema` in the same file
3. Modify your `PATCH` handler for `/review/:reviewID/:paperID` to also send a logging message
	- Send a `POST` request to `/api/message/add` (the base URL depends on how the message queue API is hosted) with a body of:
		- `userID`: the reviewer's ID
		- `time`: the current time (E.G `Date.now()`)
		- `success`: `true`
		- `data`: your filled-in schema from the previous step

# Running

1. Follow the instructions in `README.md` to get your environment set up
2. `cd packages && docker compose up --build` to rebuild anything that changed and run the app