# Purpose
- Tracking what assignments are due into an overview page
- Making it easy to jump to details on a given assignment

# Usage
Create a note in `assignments/` with the following keys in the metadata:
- `class`: the name of the class
- `deadline`: the date it's due
	- Should be in UTC format: `yyyy-MM-ddThh:mm`
- `difficulty`: how tiring the assignment will be to work on
	- May be _Easy/Medium/Hard_
- `time`: an estimate of how much time it will take to complete
	- May be _Day/Few days/Week/Caffeine demon_
- `progress`: how far along the assignment is
	- May be _Not started/Started/Nearly done/Finished_

It's recommended to use the [[Assignment]] template to add information about the assignment. This will not be displayed on the tracker

## Optional fields
- `marks`: how many marks you got on the assignment
- `max-marks`: the maximum possible marks for the assignment
	- Required when `marks` is present to calculate the percentage score

# Example
![[1A Essay]]