---
tags:
  - Software/Methodology
  - Software/Communication
---
# Overview
- Small meetings within a team to keep all members updated
- Aims to reduce risk due to changing requirements with frequent communication and smaller units of work
![Scrum workflow diagram](https://d2ds8yldqp7gxv.cloudfront.net/Blog+Explanatory+Images/scrum+workflow+1.webp)

# Roles
- Not all roles are required
	- [[#Scrum master]] and [[#Product owner]] are critical, but other roles may be employed as required
## Scrum master
- Mentor
- Aims to improve the team's process and find solutions for roadblocks

## Product owner
- Handles negotiations between the team and the clients

## Team manager
- Assigns tasks to team members
## Quality assurance manager
- Aims to reduce bugs in the software
- Does [[Code Reviews]], examines [[Automated Testing|test coverage]]

## Toolsmith/DevOps
- Manages infrastructure such as the environment and [[Continuous Integration]]

## Chief architect
- Manages architecture decisions

## User eXperience designer
- Responsible for creating a friendly UI
	- Often specialised in [[HCI Map|HCI]]
- Works with customers and users

## Developers
- Average software developer without a particular role

# Sprints
- A unit of work
- A sprint lasts 1-3 weeks on average
	- If sprints are longer, the requirements may evolve during the sprint
	- This may make some work redundant
- Starts with a planning meeting, ends with a review and retrospective meeting
- Continuous [[Scrum]] meetings happen throughout the sprint
# Releases
- An update to the software
- The contents of the release are planned in a "release planning meeting"
- A release is typically produced in 2+ [[#sprints]]

# Meetings
## Project launch
- Occurs at the start of the project

### Aims
- Find out what the minimum viable product is
- Set long-term goals 
- Make [[User Stories]]
	- Use these to create tasks --> create [[Issue Tracking#Backlog|backlog]] of [[Issue Tracking#Issue|issues]]
	- Then [[Issue Tracking#Triage|triage]]

## Release planning
- Plan the features and enhancements intended for each [[#Releases|release]]
	- These will correspond to issues in the [[Issue Tracking#Backlog|backlog]]
- Set milestones
- Expect that the requirements will change over time

## Sprint planning
- Set goals
	- Features
	- Performance improvements
	- Bug fixes
	- Refactors
- Pick issues from the [[Issue Tracking#Backlog|backlog]]
	- Ensure that there is enough time to complete these issues using:
		- [[Issue Tracking#Triage#Cost Estimates|cost estimates]] and [[Issue Tracking#Triage#Priority|priority]]

## Stand-ups
- Conducted daily or weekly
- Should be short (10 minutes, 15 minutes at most)

Each team member is asked a series of questions:
- What have you completed this week?
- What are you currently working on?
- Are you blocked by anything?

## Sprint review
- Happens at the end of a [[#Sprints|sprint]]
- Deliver (I.E deploy) and demonstrate the new features
- Summarise results
	- What was completed?
	- Were any extra things completed?
	- Were any features dropped?
- Explain any deviations from the plan
- Think about new features that could be added

## 