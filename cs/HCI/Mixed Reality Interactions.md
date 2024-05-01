# Overview
There are a variety of methods and challenges when designing [[Mixed Reality]] experiences

# Input devices
## Controllers
![A mixed reality controller](https://preview.free3d.com/img/2017/09/2174839878989120966/mwsrv878.jpg)

Pros:
- Easy to track
- Tactile
- Easy to use
- Provides some hand-tracking
- Sensing both hands expands the possible interactions

Cons:
- Not hands-free
	- You have to hold chunky controllers
- The format factor might be inconvenient for some interaction [[cs/HCI/Context|contexts]]

### Metaphors
- Using a controller that looks like the thing you're simulating can make interactions more intuitive
	- E.G a golf club, a pen

## Hands
Using the user's hands

Pros:
- Expressive
- Precise
- Direct manipulation
- More [[Virtual Reality#Immersion|immersive]]
- Showing your hands on screen improves usability

Cons:
- Hard to track
	- The interaction has to be designed around this constraint
	- Not very tactile

## Body
- Sensing the user's body
- Usually inferred from the headset's position
- Improved [[Virtual Reality#Immersion|immersion]] and [[Virtual Reality#Presence|presence]]

# Designing interfaces
- Content positioning depends on whether you're using [[Mixed Reality#Spatial relationships|egocentric or exocentric positioning]]
- Your input modality
	- [[#Controllers]]
	- [[#Hands]]
	- Head direction
	- [[#Gaze]]
- Targeting/selection can be implemented with [[#Collision-based interaction]] or [[#Ray-casting-based interaction]]

# Collision-based interaction
- When the user's controller collides with the virtual object
	- Classified as the user's controller intersecting with the object's mesh
	- Examples: pressing a button, picking up an item, dragging a slider
- Content must be reachable

# Ray-casting-based interaction
![Raycasting example](https://www.mdpi.com/applsci/applsci-09-03078/article_deploy/html/images/applsci-09-03078-g001.png)

- User point at the object
	- A virtual ray is projected from the controller or headset
		- Uses the position and direction vector of the controller
- The ray collides with the object
- Content doesn't need to be reachable
	- Content can be interacted with from a distance
- Can reduce arm [[#fatigue]]

## Ray shapes
- Arcs
![Arc ray](https://media.licdn.com/dms/image/C4E12AQG_MakEu9oUYA/article-cover_image-shrink_600_2000/0/1520142077028?e=2147483647&v=beta&t=mS2M_1eQZ-mhQ55r-JnEZWqOdbJNSFEtC1qUuFk6cVA)

- Anchored rays
	- Anchored to some object
	- Allows you to restrict the angle of interaction
- Area colliders
	- Selecting an area instead of a single point


## Input events
When should select events be emitted?
- Immediately upon collision?
- After [[#Dwelling]]?
- When in a particular input mode?
	- The user could select pinch or push mode
		- Like Photoshop but VR

## Dwelling
- When a user hovers over an object for some period of time before the select event is emitted

# Movement
AKA locomotion

Motivation: the real-world environment is bounded, but the virtual one is not. How can the user move around in the virtual world?

Solutions:
- Translational gain: change the ratio of real-world to virtual world distance
	- E.G 2 real metres = 4 virtual metres
- Redirected walking: make the user turn slightly faster in the virtual world
	- This makes the user walk in circles
- Teleportation: allow the user to teleport
- Controller (E.G joystick)
- 360 degree treadmill

# Design considerations
- [[#Reach]]: what can the users reach *with minimal effort*?
- Access: how do users access the objects
- Occlusion: do objects block each other?
- Activation: how to users trigger input events and state changes?
- Physical space: moving in the physical and virtual world at the same time

# Heuristics
- [[#Ergonomics]]: making interactions comfortable
- Legibility/readability: being able to see and read content
- [[#Awareness of reality]]: avoiding unsafe IRL behaviour
	- Falling over
	- Bumping into things

## Reach
- Falls into ergonomics
- Is the content close enough to interact with?
- Is it close enough to read?
- If objects are far, steps can be taken to make them reachable:
	- Use [[#Ray-casting-based interaction]]
	- Bring the object closer
	- Use an [[Mixed Reality#Egocentric|egocentric]] layout

## Range of motion
- Falls into ergonomics
- Are the interactions physically comfortable?
	- Content might be too high/low
	- Excessive head rotation puts strain on the neck
	- Too much arm or body movement
- Design for the user's anatomy
	- Short users may find some interactions
	- People with short arms can't reach as far

## Fatigue
- Falls into ergonomics 
- How quickly the user will get tired
- Avoid prolonged arm-raising ("gorilla arm")
	- Keep elbows close to torso during long interactions
	- Use [[#Ray-casting-based interaction]] to reduce reaching
	- Reduce [[#Dwelling#dwell time|dwell time]] or use mode switches to reduce interaction duration
- Measurable with the [consumed endurance model](https://dl.acm.org/doi/10.1145/2556288.2557130)

## Legibility
- Does it contrast properly with the background?
	- Particularly difficult in [[Augmented Reality]]
- Consider the user's perspective

Legibility can be improved by:
- Adapting the alignment to the user's perspective
- Adjusting text colour to ensure good contrast with the background
	- Can also use a shaded panel in the midground
- Render on top of less cluttered surfaces
	- Blank walls
	- Table surfaces

## Feedback
- [[Mixed Reality]] experiences often provide little sensory feedback beyond visual
- Incongruence between senses is disorienting

Solutions:
- Mid-air haptics
	- Uses ultrasound to provide touch feedback
- Wearables
	- E.G motors that vibrate when you're punched in a fighting game
- Pseudo-haptics
	- Illusions that trick you into feeling feedback

## Awareness of reality
- Virtual content can block out real objects
	- Reduces awareness of the user's environment
- Reality can intrude on virtual reality
	- E.G someone tapping your shoulder or hearing something
- Providing awareness is a trade-off against [[Virtual Reality#Immersion|immersion]] and [[Virtual Reality#Presence|presence]]

### Obstacle awareness
There are a few strategies for providing awareness:
- Bring important real objects into virtual reality
	- Cars
- Constrain the area
	- Introduce a calibration step before the user can interact
	- The user must draw the boundaries of the usable space
	- The system will warn the user if they go out of bounds
	- Drawback: this further reduces immersion because the virtual world will likely be bigger than the selected area
	- Solution: generate the virtual world to match the real environment
