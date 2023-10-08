# Purpose
- Tracking what assignments are due into an overview page
- Making it easy to jump to details on a given assignment

# Usage
Create a note in `assignments/` with the following [inline fields](https://blacksmithgu.github.io/obsidian-dataview/annotation/add-metadata/#inline-fields):
- `class`: the name of the class
- `deadline`: the date it's due
	- Should be in UTC format: `yyyy-MM-ddThh:mm`
- `difficulty`: how tiring the assignment will be to work on
	- May be _Easy/Medium/Hard_
- `time`: an estimate of how much time it will take to complete
	- May be _Day/Few days/Week/Caffeine demon_
- `progress`: how far along the assignment is
	- May be _Not started/Started/Nearly done/Finished_

In the [front matter](https://help.obsidian.md/Editing+and+formatting/Metadata), there must be the following keys:
	- `semester`: which semester within that year the assignment belongs to. Must be in the range $1..2$
	- `year`: which year in my degree this assignment belongs to. Must be in the range $1..4$

It's recommended to use the [[Assignment]] template to add information about the assignment. This will not be displayed on the tracker

## Optional fields
- `marks`: how many marks you got on the assignment
- `max-marks`: the maximum possible marks for the assignment
	- Required when `marks` is present to calculate the percentage score

# Example
```
---
when:
  - year: 1
  - semester: 2
---

class:: "Sociology 1A"
deadline:: 2022-11-24T12:00
difficulty:: Medium
time:: "Caffeine demon"
progress:: "Not started"

marks:: 20
max-marks:: 120
```