---
semester: 2
year: 3
---
class:: "MHCI"
deadline:: 2024-03-11T16:30
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
	- On target, segment gets larger
	- On activate, vibrate, make the button's border glow, pop forward, then the menu closes after a delay
		- Use [browser vibration API](https://developer.mozilla.org/en-US/docs/Web/API/Vibration_API)

## Input modality
In this research, I will focus on quick-access menus for mobile games, drawing and editing software, and mixed reality games. For my input technique, I will use touch because it is readily available on mobile phones. I had hoped to use hand tracking, but opted for touch instead as I could not access the appropriate sensors. I 

While I will be focusing on touch, I believe that my input technique would work for both hand-tracking and joystick input modalities. As such, I will make describe how the input technique could be adapted for those modalities throughout this paper.

Input technique

I have chosen to create a radial marking menu for the following reasons:
1. A marking menu minimises the distance from the cursor to all buttons
	1. This contrasts with traditional context menus which create a large distance between the click and the bottom-most menu item
2. Marking menus spring out from a single point. This property can be leveraged to create contextual interactions such as editing a particular object in a drawing
3. Marking menus are ideal for pen inputs because they easily glide from the centre-point to the buttons. This research aims to uncover whether such menus are also ergonomic for other modalities such as touch, hand-tracking, and joysticks

## Use cases
- Quick-access
	- You won't be able to have lots of items as they have to be close to the centre
- Inventories in VR games
- Favourite tools in drawing apps
- Joystick input devices

## Input modalities
- Touchscreen
	- Hold tap --> drag --> release
- VR hand tracking
	- Same as touchscreen
- Joystick
	- Click --> rotate --> release

## Prototype ideas
- No delay on long-press
- Long-press allowed anywhere
- Long-press allowed when mode selected
- Long-press allowed in certain region
	- Corner (would limit the number of items)
	- Could be better for one-handed interaction
- Lots of buttons

# Bibliography
- [Marking menus](https://dl.acm.org/doi/abs/10.1145/191666.191759)
- [Cursor shift](https://dl.acm.org/doi/abs/10.1145/1240624.1240727)
- 
