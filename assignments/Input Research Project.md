---
semester: 2
year: 3
---
class:: "MHCI"
deadline:: 2023-03-11T16:30
difficulty:: Low
time:: "Week"
progress:: "Not started"

# Pre-requisites

# Requirements
- Design and implement an input method for clicking buttons that:
	- Does not directly use touch
		- You can't just tap the button
		- Touch is allowed to facilitate the interaction
		- Or if you're doing something creative with it like a marking menu
	- Is grounded in academic research
	- Allows the user to select from multiple buttons
- A report describing the design process (up to 6 pages)
	- Why the input modality was chosen
		- What modalities were available for the chosen device?
	- How false-positive/negatives are handled
	- How feedback is given to the user
		- How do they know that they are interacting with the system?
			- Correctly?
		- Reporting button activation
	- Interaction discovery
	- How \[un\]intentional actions are recognised
	- Constraints on the button design and layout
		- Maximum number of buttons?
	- Prototypes that failed
	- Pros/cons
	- Use cases
		- Could it be adapted to work for different devices/layouts/button sizes/feedback types?
	- Comparison to other techniques
- Conduct a user evaluation
	- Include the results and changes made in response in the report
	- Compare with tap inputs
- A finite-state machine
- Code submitted via GitHub
- 

External requirements [here](https://moodle.gla.ac.uk/pluginfile.php/7898929/mod_resource/content/2/Coursework%20Handout.pdf)

# Helpful links

# Research
## Ideas
- A marking menu for an inventory
	- Long press in the middle of the screen to reveal
		- Button in the bottom right corner for discovery
		- If no interaction for a while, show an animation of a long press
	- Expand a radial menu around the centre
		- A series of concentric circles
		- Buttons get larger as they get further from the centre
	- Ideal for quick-access
		- Not good for displaying a full inventory
	- Release to activate
	- On activate, vibrate, make the button's border glow, pop forward, then the menu closes after a delay

# Bibliography
- [Marking menus](https://dl.acm.org/doi/abs/10.1145/191666.191759)
- [Cursor shift](https://dl.acm.org/doi/abs/10.1145/1240624.1240727)
- 