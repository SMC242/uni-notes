---
tags: HCI/Mobile 
---
# Overview
Touchscreens are convenient, but have many issues. Mobile apps have to be designed around these problems

# Input
- The screen is used for both input and output
	- The user's hand can cover the output ("occlusion")
- The screen will be small
	- Fingertips might be bigger than elements on the screen
	- Reduce target density to prevent misclicks
	- See also: [[Fitts' Law]]

## Solutions
### Shift
- A technique for dealing with inaccurate or ambiguous inputs
- A zoomed lens appears after a tap-and-hold gesture
	- Used by iOS text inputs

### Back-of-device
- Technique for mitigating screen occlusion
- Touchpads the back of the device

### Stylus
- More accurate than fingers
- Occludes the screen less

# Posture
- The way the user is holding their device affects which interactions are easy
	- Phone in right hand: poor access to the top left
	- Phone cradled in left hand: bad access to bottom right, worse access to right and bottom halves
	- Horizontal, two-handed grip: good both sides, bad middle
- Fingers can only reach so far

## Solutions
### Layout
- Optimise the layout for predicted posture
	- Put common actions in the easy zones
- Make targets in the hard-to-reach zones bigger

### Gestures
- Swipe
- Pinch
- Multi-tap
- Drag
- Spiral

### Posture detection
- Detect the current posture and adapt the layout
	- If the device is horizontal, shift the layout for a two-handed grip