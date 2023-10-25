| Team ID   | 3F                                                                                                                                         |
| --------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| Figma URL | https://www.figma.com/file/4ZAR4mPmFkjJ1CkLuK8Aen/2914063k's-team-library?type=design&node-id=2479%3A3553&mode=design&t=a8WZBuWmOotC69ob-1 |
|           |                                                                                                                                            |
 
# Scope
We decided to rework the most egregious pages, namely: Add By Requirements, Make A Payment, Student Homepage, My Choices, and the course information view. Each page has its own key issues:

- Add By Requirements: the monolithic accordion drastically slows down traversal through the courses
- Make A Payment: payments are duplicated and it is difficult to enter the amount you would like to pay
- Student Homepage: the icons use inconsistent art styles and are not aligned properly
- My Choices: lack of feedback about conflicts and poor help for solving them
- Course information: the information is dense and hard on the eyes

Additionally, we decided to rework the navigation bar to aid traversal between sections of the site. Previously, it only allowed you to go to Student Homepage and had invisible buttons (a client-specific bug we were unable to consistently reproduce).

# Changes
## Add By Requirements
- Only the classes that are available to the user for this year are shown
	- Reduces cognitive load by displaying only the relevant information
	- The downside is not allowing users to look ahead at the classes for future years. The [main website's course list](https://www.gla.ac.uk/coursecatalogue/courselist) page can fulfil this purpose instead
- A timetable tray was added to the right hand side of the page
	- Provides a preview of the user's standard week
	- Gives immediate feedback, aids planning, and helps the user avoid conflicts
- Classes are listed in a more modern and sparse way
	- Reduces eye strain

## Make A Payment
- Add radio button on every payment charges for user to select which charges they want
	- Previously, the user had to click "zero all" before typing their desired values
- Show list of charges user selected on the confirm payment page
	- Only show the total amount to be paid

## Student Homepage
- Notifications added
	- Provides feedback for users and points them towards urgent issues
- Navigation buttons made more consistent
	- Art style and alignment
- Added timetable overview
	- This and the other changes centralise all key information into one view

## My Choices
This page was not completed by the team member

## Course info
- Less important information about courses (room, teacher) moved to pop-out menu
	- Revealed by clicking the "more information" button next to a course
	- Reduces information bloat on the screen, still allows access when required
- Pop-over shopping-cart that displays the courses in "My Choices"
	- Shows the current choices without navigating to another page
	- Reduces memory overhead as users can quickly see their choices

## Miscellaneous
- Added a consistent navigation bar
	- It provides access to all sections of the site
	- Makes it easier to jump between parts of a workflow

Some icons were used from this icon sheet:
https://www.figma.com/file/zrn9DL2wrckN32yAG5zD17/Free-Icon-Pack-1600%2B-icons-(Community)?type=design&node-id=2165-2518&mode=design&t=0D7fkUoY5cAB38Uj-0