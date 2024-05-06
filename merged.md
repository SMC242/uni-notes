---
tags:
  - Research/Method
---
# Overview
Randomly showing either the existing version ("control") or a new version ("treatment") when a user visits part of an app. This allows developers to get feedback from real-world use without committing to a full deployment the design

# Use case
- Checking for causal relationships between treatments and some metric
- Not much data to prove/disprove the utility of new designs

# Problems
- Agreeing on a metric (OEC - Overall Evaluation Criteria) is difficult
	- Revenue is often used
- Doesn't tell you why a design is better or worse
- Primacy effect: even if a new design is better than the old, it may make the user experience worse for a while
	- I.E the user has to learn the new design or other systems need to be updated to work it
- Contamination: users may have multiple devices or block whatever tracking mechanism you're using to assign test groups
	- This means that one subject may be counted many times
	- Particularly likely for regular users as they might be more likely to use multiple devices (my speculation)
- 

# Ramp-up
- Don't start with a 50-50 split between the control and the treatment
	- Too much risk
	- There could be massive issues that break production

1. Start with a small chance for the treatment
	- 0.1%
2. Check for issues and fix them
3. Increase likelihood of encountering treatment
4. Repeat until 50%

# Overview
- Some actions take more time than others
- These are grouped into bands
- These groups are used to categorise the focus of heuristics and research questions 

# Biological band
100 $\micro$s - 10 ms
- Movements that a user's body controls subconsciously

# Cognitive band
100 ms - 10 seconds
- Actions that happen subconsciously
	- Often involves reaction time
- E.G typing, clicking, tapping, swiping

# Rational band
Minutes - hours
- Tasks that you consciously think about
- Experiencing an interface and making decisions about the next action
- E.G navigating websites, searching, picking search results

# Social band
Days - weeks
- Actions involving other users
	- Involves social norms and bonds
- E.G meetinmgs, privacy, messaging---
tags: HCI/Mobile 
---
# Overview
A variety of factors affect adoption of technologies. This is particularly true in social scenarios because users may feel judged

# Performative interaction
- Users feel like they're being observed all the time ("spotlight effect")
- They adjust their behaviour to be socially acceptable
- They also have their own feelings about using the device
	- They might think it looks silly

> [!EXAMPLE] Google Glass
> As the Google Glass had a camera in it, non-users felt like they were being recorded by users. This led to stigma around the product and it was not adopted

## Spectators
- The spectators might not understand what the user is doing
- They might judge the user

## Factors
Some factors that influence social acceptance:
- Visibility (E.G using the device draws attention)
- Setting (using the device at inappropriate times)
- Comfortability with using the device
- Impressions about the device
- Familiarity
	- People are more likely to reject things they don't know
- Behaviour
	- Devices may cause antisocial behaviour

# Increasing social acceptance
## General rules
- Avoid unusual inputs/outputs
	- Make it clear what the user is doing to reduce judgement from others
	- E.G touching your nose, strange noises, looking around
- Avoid inputs/outputs that draw attention
	- Loud noises
	- Big movements
	- Flashing
- Avoid actions with unclear targets
	- Shaking device
	- Voice-activated actions can be unclear
	- Camera in glasses (spectators feel like they're being recorded)
- Reduce visibility
	- Use vibration instead of audio
- 
## Spectators
- Recognition may improve spectator acceptance
- It helps if there is a visible outcome after the interaction
	- This helps the spectator to figure out that the user is interacting with a device

## Gesture design
Gestures that follow these rules are more likely to be accepted:
- Hands near torso
- Small movements
- Short duration

Gestures following these rules are less visible

## Device location
These device locations are more likely to be accepted:
- Wrist
- Forearm
- Hands

These device locations are less likely to be accepted:
- Neck
- Waist 
- Torso---
tags: HCI/Mobile 
---
# Overview
Users pay varying levels of attention depending on their situation

> [!NOTE] Key concept: fragmented attention
> A user's attention is divided between the device and their surroundings.



See also: [[Context]]

# Multi-tasking
- Multi-tasking heavily affects attention
	- Attention span
	- Spatial and social awareness
	- Fragmented attention

## Task switching
- Users pay attention in bursts
	- 4 - 14 seconds
- Switches occur frequently

# Designing for mobile
- Mobile apps are often used in distracting environments
- When designing mobile apps, you need to expect:
	- Lack of visual attention (not looking at the screen all the time)
	- Not noticing changes ("change blindness")
	- Interruption
	- Fragmented input (E.G typos, infrequent inputs)
	- Slow responses
- Avoid requiring too much attention

## Heuristics
- Clear: make it easy to find things \[with minimal reading\]
- Patient: don't demand constant attention
- Simple: don't overwhelm the user with features
- Multi-sensory: reduce reliance on visual attention
	- Use vibration, light, sound
- Flexible: support multiple input modalities

# Safety
- If a device is using too many cognitive resources, they may stop using it
- However, they might tunnel-vision on it instead
	- Could be dangerous or disruptive
---
aliases: AR
---
# Overview
When virtual experiences are layered (I.E overlayed) on top of reality. The majority of the user's experience is real

# Augmented virtuality
When the user's experience is mostly virtual, but there are some real elements (E.G interacting with a real keyboard that is also present in the AR view)

# Modalities
## Handheld
E.G phones

Pros:
- Availability: you already have a phone
- Agency: you can put it away
- Portable

Cons:
- Limited screen size
	- Less immersive
- Not all sensors might not be available on the user's device
- [[Mixed Reality#6 DoF|6 DoF]] tracking hasn't been developed yet

## Overlay
E.G glasses

Pros:
- Readily available
- Only [[Mixed Reality#3 DoF|3 DoF]]
- Minimal hardware required

Cons:
- Limited interaction (fewer sensors)
- Blocks vision
- Limited immersion
- Can't be shared by multiple users at once

## Immersive
E.G headsets

Pros:
- Readily available
- Supports [[Mixed Reality#Perspective-dependent rendering|perspective-dependent rendering]]
- Can be both egocentric and exocentric at the same time

Cons:
- More technically expensive
- Bulky
- Expensive

## Auditory
E.G earphones, glasses with speakers

Pros:
- Readily available
- Minimal hardware required

Cons:
- Not very discoverable
- Limited interaction# Motivation
- # Overview
- Consistency means following conventions within your software
- Users will be confused when things change arbitrarily
	- They may think it has a different purpose when it does not

> [!NOTE] Treat like elements the same
> Elements that fulfil similar purposes should be treated the same. If one is more important, you may change aspects of it, but only up to 2 aspects (E.G colour and shadow)

# Types
## Internal
- Choices made within the app should apply throughout the app

## External
- Consistency with common patterns in similar software

# Elements of consistency
- [[Layout]]
- Typography 
- [[Colour]]s
- Imagery
	- Consistent logos, icons, art styles
- Control and feedback
	- Use the same UI elements across the app---
tags:
  - HCI/Mobile
---
# Overview
Devices can detect what the user is doing with its sensors

- A user might be walking if they're moving at ~1.42 m/s
- A user might be on a train if they're moving quickly along the path of a train line

See also: 
- [[cs/HCI/Context|Context]]

# Pitfalls
- Bad inference can cause frustration
- Privacy

# Examples
## Phone in pocket
- Context: {dark, motion, last touch 240 seconds ago}
- Inference: {phone in pocket, user is walking}
- Action: use text-to-speech for navigation instructions

## User is busy
- Context: {meeting in calendar, flat device, ambient noise}
- Inference: {phone on table, user is in a meeting, other people are around}
- Action: show notifications on smart watch with vibration and icons - no sound

# Ambiguity
- There may be multiple reasons for a context
- Quality can be bad
	- Poor connection
	- Poor GPS signal
- Information can be ambiguous
	- Movement: walking or cycling?
	- Light: night time or in user's pocket?
	- Movement: rapid movement = car or bus or train?
- Try to use multi-faceted sensing to overcome ambiguity

## Feedback
- Show ambiguity
	- E.G Google Maps shows the GPS accuracy on the map
- Tell the user what the system thinks is happening so they know why it's making its decisions---
tags:
  - HCI/Mobile
---
# Overview
Mobile devices are used in many different situations. These situations ("contexts") affect which features are used and how they are used

# Constraints
## Environment
The environment affects which features can be perceived

- A train vibrates a lot, so haptic feedback may not be noticed
- Light themes hurt users' eyes in dark environments
- Notification pings might not be heard in a concert

## Motion
- People don't want to look at their phones while they're walking
- Typing while walking is difficult

## Connectivity
- Connection may fluctuate as a user moves (E.G under a tunnel)
- They may turn off their data whenever possible

## Situational demands
Constraints introduced by the user's current activity

- A user may have other things in their hands
- They might not be able to pay full attention 
- They might be driving, so looking at the device must be minimised

# Situation impairment
- When a context makes it harder to use a feature of a device

> [!EXAMPLE] Examples
> - Holding a page in one hand
> 	- Only one hand is available to interact
> - Bright environments
> 	- Screen glare makes it harder to see content
> - Busy street
> 	- Can't hear audio
> - Walking
> 	- Need to watch surroundings# Overview
While it's impossible to avoid errors entirely, it's best to limit the number of possible errors

# Solutions
- Find the sources of errors
	- Once found, find ways to minimise them
- Allow reverting actions
- Aid error discovery (provide help to understand the problem)
- Make errors easier to fix
- Use [[#forcing functions]]

# Forcing functions
- [[Intuitive Design#Constraints|Constraints]] that force a behaviour
- Can be annoying

> [!NOTE] Interlocks
> A forced sequence of actions
> 
> Example: you must create an account before signing in

> [!NOTE] Lock-ins
> Preventing an action from stopping
>
> Example: you cannot shutdown your computer while a file lock is held

> [!NOTE] Lockouts
> Preventing an action
>
> Example: Barriers prevent people from riding their cars off cliffs
---
tags: Research Ethnography 
---
# Overview
[[Ethnography]] is a useful method for understanding how systems are used in reality

# Motivation
- Understanding how systems are used
	- The social context
	- Unanticipated use cases
- Understanding practices
	- Practices are the unit of ethnographic HCI (instead of interactions)
	- Practices are processes that occur at a particular time in a particular space
	- Cultures and values
- Used in multiple stages of the design process
	- Early: to gather requirements
	- Later: find out how the system is being used once it has been deployed in order to improve it

# Doing ethnography
- Don't assume your experience is universal
- Your study should be rooted in a particular context
	- The organisation
		- Its work practices
		- Values (of the company and of its members)
		- Interactions between people
	- A group of people

# Comparison to surveys
<ul class="breakdown">
	<li class="pro">Can ask deeper questions</li>
	<li class="pro">People don't give detailed answers to surveys (memory is poor, explaining is hard, they might not understand what they do, may lie)</li>
	<li class="pro">If in the early stages of the design process, you might not know what to ask</li>
	<li class="con">Surveys are cheaper</li>
	<li class="con">Requires many skills and resources (access to field, selecting the right questions and subjects, conversational skills, data analysis)</li>
	<li class="con">Expensive</li>
</ul>

See also: [[Questionnaires]], particularly [[Questionnaires#Bias|bias in questionnaires]]
# Overview
Fitts' Law says that its easier to find things that are big and close to where you currently are

![[fitts-law-example]]

# Formula
$$
T = a + b \log_{2}\left(1 + \frac{D}{W}\right)
$$
where
$$
\begin{flalign}
&T = \textrm{time to complete movement}&&\\
&a = \textrm{cognition constant}\\
&b = \textrm{hand-eye coordination constant}\\
&D = distance\\
&W = \textrm{target width}
\end{flalign}
$$

# Difficulty
"Index of task difficulty"
- A metric for task difficulty
- Unit: bits

$$
ID = \log_{2} \left(\frac{D}{W} + 1 \right)
$$

# Performance
"Index of performance/throughput"
- The rate of actions

$$IP = \frac{ID}{T}$$
# Use case
- Predicting movement time
- Comparing input devices
- Placing elements in a view

## Design implications
- Make elements easier to move to by making elements bigger and closer together
- Right-click menus show context actions in-context
- Put things on screen edges
	- Particularly corners
	- Requires less accuracy---
tags: HCI/Mobile 
---
# Overview
Gestures are powerful for making intuitive interactions, but they have problems with discovery

Users might not know:
- Which gestures are available
- Where/when to use them
- How to do the gesture correctly
- How to troubleshoot
	- Improve their performance of the gesture

# Discovery
## Opt-in
- Gives an opportunity to show the user how to do the gesture
- Avoids accidental gestures
- More efficient: the relevant sensors won't be active unless the user wants the gesture to be available

# Feedback
- Give as much feedback as possible
	- Visual
	- Audio
	- Vibration
- Confirm that the system is responding to a gesture
- Visualise the data that the sensors are using
	- Hand silhouette on handprint sensor# Overview
Human Computer Interface is a multi-disciplinary field focusing on improving user interfaces. Closely related to [[Design Map|Design]]

![Discipline Venn diagram](https://aelaschool.com/wp-content/uploads/2023/03/Human-ComputerInteractionEverythingYouNeedToKnow_Imagem1_d051c2827e0b0d6ca172ec6387e76a24_800.png)

# Design principles
Ideas that guide web design

- [[Visual Design]]: making designs visually appealing
	- [[Colour]]
	- [[Consistency]]
	- [[Layout]]
	- [[Hierarchy]]
- [[Fitts' Law]]: making elements easier to click on
- [[Error Prevention]]: avoiding errors
- [[Intuitive Design]]: guiding users along the correct paths
# Mobile Devices
#HCI/Mobile

Mobile devices have their own design constraints

- [[cs/HCI/Context|Context]]: how the situation a device is being used in affects its usage
	- [[Context-aware Computing]]: designing features around contexts and activating them when those contexts are detected
	- [[Attention]]: factors affecting a user's focu
- [[Sensors]]: the sensors available to mobile apps
	- [[Touchscreens]]
- [[Gestures]]: designing good gesture-based interactions
- [[Adoption]]: factors that determine whether a technology will become popular
- [[High Mobility Design]]: designing for users on the move

# Mixed reality
![[Mixed Reality#Overview]]

- [[Mixed Reality]]
	- [[Mixed Reality Tracking]]
- [[Augmented Reality]]
- [[Virtual Reality]]
# Research
Research is core to the field of HCI. The field has drawn heavily from social psychology and has adopted many of its methods

- [[Hypothesis Testing]]: proving whether there is a significant difference
- [[Ethnography]]: a research method that utilises immersion to understand the phenomenon from an insider's perspective
	- [[Ethnography In HCI]]
- [[Questionnaires]]: designing good surveys
- [[A-B Testing]]: comparing designs against each other# Overview
---
tags: HCI/Mobile 
---
# Overview
There are a variety of extra consideration when designing for users who are quickly moving (cyclists, drivers, runners)

> [!NOTE] Key concepts
> ![[Attention#Safety]]

See also:
- [[cs/HCI/Context|Context]]
- [[Attention]]

# Common contexts
## Cycling
Attentional demands:
- Road users
- Pedestrians
- Traffic signals
- Surface, terrain
- Road rules
- Navigation
- Obstacles

Physical demands:
- Exertion
- Steering
- Braking
- Balance

Situational impairments:
- Hands busy
- Low stability
	- Reduced input precision

## Running
Attentional demands:
- Pedestrians
- Traffic
- Terrain
- Road crossing
- Navigation
- Obstacles

Physical demands:
- Exertion
- Walking posture ("gait")
- Balance
- Arms moving

Situational impairments:
- Handheld and wearable devices have no stability
- Instability due to gait

## Driving
Attentional demands:
- Pedestrians
- Other cars
- Traffic signals and rules
- Navigation
- Obstacles
- Dashboard devices

Physical demands:
- Control
- Gear changes
- Signalling

Situational impairments:
- Hands busy

## Common attributes
Attention:
- Multiple related cognitive demands
- Increased attentional demand and cognitive load
- Users need to switch attention instead of dividing it
	- Cognitive/attentional draw from the activity is too high

Situational impairments:
- Can't hold/touch devices
- Physical demands
- Other [[cs/HCI/Context#Situation impairment|contextual impairments]] like noise and light

# Solution
Solutions for easy inputs in mobile contexts

## Principles
- Anticipate the user's interactions being limited
- [[Attention#Designing for mobile|Minimise visual attention demands]]
- Avoid touchscreens. Other modalities offer:
	- Lesser visual attention demands
	- Less physical contact required
		- Can be interacted with quickly
	- Less precision required

## Physical controls
- Offers [[Intuitive Design|tactile cues]]
	- You can find them without diverting visual attention
	- Offers tactile feedback on activation
- Requires less stability and precision
	- Users can feel around until they find the control
	- They can grasp the surrounding area to stabilise device/input

## Touchscreen gestures
- Less visual attention required
	- Don't need to locate targets
- Less precision required
	- Easy in most situations

## Contactless interactions
- E.G mid-air gestures and voice controls
- Less visual attention required
- Drawbacks typical of [[gestures]]

# Output modailities
- Audio or haptic feedback
	- Doesn't require visual attention
	- Can't always see or feel it
	- Needs a fallback in case it's ambiguous
- Keep visual feedback in front of the user
	- Avoid user looking away from their task
	- E.G [[Augmented Reality|AR]] glasses, heads-up displays# Overview
When approaching new systems, users use the following tools to understand them:
- [[#Affordance]]
- [[#Knowledge]]
- [[#Constraints]]
- [[#Convention]]

# Signifiers
- Used to signal that something is there
- E.G a sign, an image, a theme song
- Two types:
	- Deliberate: things that have been created on purpose (signs)
	- Emergent: things that were coincidentally created (well-trodden paths, queues)

# Affordance
- What the individual thinks the system is for based on its appearance
	- E.G a chair might be for sitting
	- Not all people will see the same thing. A knife can mean multiple things - a weapon, a kitchen tool, a survival tool
- Forms a relationship between the object and an individual
	- The affordance of an object is not a property of the object itself
	- It's an interpretation of the object

> [!NOTE] Example parameters
> - Materials
> - Textures
> - Size
> - Height
> - The individual's capabilities (someone in a wheelchair may not see stairs as an entrance)
> - The individual's past experience

## Anti-affordances
- Objects that exist to prevent interaction
- E.G speedbumps, anti-homeless benches, anti-pigeon spikes on ledges

# Knowledge
- Things that an individual knows
- You don't need to know all of the ins and outs of a task to complete it
	- Just the broad strokes
- Two types:
	- Knowledge of the world
		- Perceived [[#affordance]]s
		- Controls
		- The actions available
	- Personal knowledge (stored "in our heads")
		- Memories
		- Past experience

## Memory
- Short term memory (STM) can only hold so much
	- $7 \pm 2$ items
	- Prone to distraction
	- Duration: 30 seconds
- Long term memory (LTM)
	- Arbitrary things
	- Meaningful things
	- Retaining and retrieving memories is easier when the information makes sense

# Constraints
- Constraints limit the possible uses of a system
- They are useful for preventing errors and guiding users towards intended behaviours

3 types:
> [!NOTE] Physical constraints
> - What is physically possible
> - An SD card will not fit in a USB port

> [!NOTE] Cultural constraints
> - Behaviours learned from others
> - Hold knife in right hand, fork in left

> [!NOTE] Semantic constraints
> - Intrinsic meaning
> - An egg is not bread, so it does not belong in the bread bin

> [!NOTE] Logical constraints
> - Reasoning
> - Cars can't drive on both sides of the road because they would collide

# Convention
- Try to be consistent with conventions
	- This makes it easier for users to go between systems
- If it's not possible to encode conventions into the design, use a [[#Constraints|cultural constraint]]
- Standardisation can enforce a convention where there is no natural choice
	- E.G little-endian vs big-endian binary encoding: CPU architectures pick one and stick with it

## Progress
- A design may be better, but people don't like to learn new things
- Only change conventions when the change provides a large and obvious benefit
- Mixed systems are confusing
- Standards inhibit future development
	- They make it harder to change things
	- Use them as a last resort# Considerations
- Screen size
	- Some designs may only work for some screens
	- The layout may need to adapt to different screens
- Position
	- Related things should be close together
- White space
	- Gaps create groupings
- Proximity, size, alignment
	- Another way to group elements
- Grid# Overview
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
- The format factor might be inconvenient for some interaction [[cs/HCI/Context|Context|contexts]]

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
	- Reduce [[#Dwelling#dwell time]] or use mode switches to reduce interaction duration
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
---
aliases: XR
---
# Overview
Mixed reality is a mix of real and virtual experiences (all senses, not just visual)

![Milgram's reality-virtuality continuum](https://www.researchgate.net/publication/321405854/figure/fig1/AS:567028507475975@1512201539715/Milgram-and-Kishinos-Mixed-Reality-on-the-Reality-Virtuality-Continuum-Milgram-and.png)

# Mixed reality tracking
![[Mixed Reality Tracking#Overview]]

# Perspective-dependent rendering
- Virtual content appears in the correct place
	- E.G a mug appears on the table in the right orientation
- Requires lots of [[Sensors]]
	- Rotation
	- Position
	- Controller tracking (sometimes)
	- Hand tracking (sometimes)

# Augmented reality
![[Augmented Reality#Overview]]

# Virtual reality
![[Virtual Reality#Overview]]

# Spatial relationships
There are two frames of reference when rendering the virtual content

## Egocentric
When the content is placed relative to the viewer

Pros:
- Portable: content moves with the user
- Consistent: content is always in the same place relative to the user's view
- Reachable: the content is always within reach

Cons:
- Integration: not part of the environment
- Obtrusive: content can obscure reality in annoying ways
- Single-user: can't be shared due to no shared frame of reference
- Instability: content can be shaky

## Exocentric
When the content is placed relative to the world

Pros:
- Integration: the content becomes part of the world
	- More [[Virtual Reality#Immersion|immersive]]
- Optimisation: the content can be moved according to the environment
- Implicit haptics (the surfaces exist already)
- Collaboration: multiple users can interact with the same content

Cons:
- Needs [[Mixed Reality Tracking#6 DoF|6 DoF tracking]]
- Access: content has to be in a fixed location
- Limited discoverability and reachability

## Choosing
It's rarely straight-forward, but here are some guidelines:

Choose egocentric positioning when:
- The content is coupled with a context local to the user (E.G a speed meter while cycling)

Choose exocentric when:
- The content is coupled with an external context (E.G displaying the menu of a restaurant)# Overview
In order to implement [[Mixed Reality]], you need to track where the user is and what they're doing
# Degrees of freedom
AKA DoF

- The number of senses the device can track

![Degrees of freedom](https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/6DOF.svg/512px-6DOF.svg.png)

## 0 DoF
- Overlay only

## 3 DoF
- Tracking rotation (x, y, z)
	- Where the user is looking
- No position (movement)
- Requires an [[Sensors#Inertial measurement unit|IMU]]

## 6 DoF
AKA room-scale tracking 

- Tracks rotation and position
- Requires an [[Sensors#Inertial measurement unit|IMU]] and a [[Sensors#Simultaneous Localization and Mapping|SLAM]]

# Strategies
There are multiple strategies for tracking the user's environment

## Inside-out
- Tracking using sensors within the device
	- [[Sensors#Simultaneous Localization and Mapping|SLAM]]
	- Controller or hand tracking

Pros:
- Self-contained
- Portable

Cons:
- Tracking quality may vary
- Relative positioning only

## Outside-in
AKA lighthouse tracking

- Using external devices
	- Laser emitters that alternate between horizontal and vertical sweeps
	- The headset and/or controllers detect and respond to the lasers
- This gives an absolute frame of reference

> [!EXAMPLE] 
> Motion capture uses this strategy
> 
> ![Motion capture suit and skeleton](https://en.nokov.com/upload/20220329174323_437.jpg)

Pros:
- Best-in-class tracking
- Absolute positioning
- Ideal for tracking multiple people
	- E.G Xbox Kinnect

Cons:
- External devices required
- Semi-permanent setup
	- Hard to move---
tags:
  - HCI/Mobile
---
# Overview
Mobile devices have a variety of sensors that can be utilised by developers. Often, multiple sensors are used in one interaction

- Accelerometer
- Gyroscope 
- Magnometer
- Specialised sensors for:
	- Temperature
	- Humidity
	- Moisture
	- Ambient light
	- Proximity
	- Barometric pressure
	- GNSS location (colloquially called GPS)
	- Heartrate
	- Fingerprint
	- Iris scanner
	- Depth
- Radar
- LIDAR (for distance to objects)

# Inertial measurement unit
AKA IMU

Contains:
- Accelerometer (movement on the X, Y, and Z axes)
- Gyroscope (rotation)
- Magnetometer (heading)

# Simultaneous Localization and Mapping
AKA SLAM

- Maps the environment in real time
- Tracks your position within the environment
- 

# Sensing
## Surroundings
Sensing immediate surroundings

- Use ambient light
- Use proximity

## Position
Sensing where the user is in the world

- Use barometric pressure
	- Tells you the elevation above sea level
- Use GNSS for location

## 3D
Use: 
- Radar
- LIDAR
- Depth

## Audio
- Microphones
	- Ambient audio
	- Speech recognition

## Video
- Cameras
	- Object recognition
	- Hand tracking
	- Eye tracking
	- Facial recognition

# Touch
- The APIs usually provide an x,y coordinate for the touch, but the sensor actually records the full contact area
- Can also detect up to 5cm above the screen

# Pressure
Types:
- Grip (squeezing the phone)
	- Using a force-sensitive resistor
- Touch (pressing the screen)
	- Using force sensors (barometric pressure sensors) below the screen

- Can be useful for adding extra states
- ---
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
	- If the device is horizontal, shift the layout for a two-handed grip---
aliases: VR
---
# Overview
When virtual experiences replace one or more sensations. The user experiences mostly virtual reality by blocking out some of their senses

# Immersion
- The sense of being inside the virtual world
- Accepting the reality as if it's real

# Presence
Has a few types:
- Social: feeling like you're with other people
- Spatial: feeling like you have agency in the world
	- The world reacts to you
- Personal: feeling like you're inside the world
	- You feel that you control your body in the world

# Core ideas
- Software should be pretty *and* practical

![[Consistency]]

![[Layout]]---
tags:
  - HCI/Mobile
---
# Overview
Wearables are devices that are worn on the body such as a smart watch or earbuds

- Unrestrictive
	- Don't get in the way
- High availability
	- Always within reach
- Aware
	- Can sense the [[cs/HCI/Context|Context]]
- Intimacy
	- It can have skin-contact which allows medical applications (E.G heartbeat sensing)
- Proactive
	- The device can take action without you taking your phone out of your pocket

# Challenges
- Small/no screen
	- Little visual output
	- Can't type
- Limited input modalities
- Movement
	- Can be difficult to interact with a small device while moving
	- Difficult to filter out movement data
		- People gesture, walk, run
- Aesthetics
	- Limited by comfort
		- Can't be heavy, bulky, or unusual
	- Must fit the user's style

# Form factors
- Wrists: watches, bracelets
- Head earbuds, glasses
- Fingers: rings
- Feet: shoes

# Input modalities
## Buttons/dials
- Physical controls
- Simple to use
- Eyes-free
- Haptic feedback
- Mapping functionality can be difficult
	- Not enough buttons for every function

## Touchscreen
- Familiar
- Input and output are coupled
- Will be tiny on a wearable
	- People will miss targets
	- Their fingers will occlude the target

## Touch panels
A panel that you can slide your finger across

![Google glass touch panel](https://lh3.googleusercontent.com/proxy/OfW7QBg5SiZhEm2TYD8cYow2gk6BEIwagJ83sb8Vbhg0YX39NBaQhPkexW-soKCsoNjQzcmeoCM3ELIrrp2t-PKA7Oo8yriMGGVee9d6qQnDjz5GGCojdknsXcB1uB3paWGb269o4e1MnNi_gQ)

- Easily integrated into glasses, wristbands, and rings
- Little visual feedback
- Miniaturised input devices are a double-edged sword
	- More portable
	- But hard to discover

## Gestures
- Best for wearable computers
- Eyes-free
- Unrestrictive
- Sensed by wearables
	- I.E an external device is required such as rings or a controller
- Can be intuitive to use
	- We're good at moving our bodies
- Tiring

### Gloves
- Directly measures finger motions
- Provides input and output
	- E.G a screen or haptics
- Many [[Mixed Reality Tracking#Degrees of freedom|degrees of freedom]]
- Lots of gestures are possible
	- Due to having more flex points (at each joint)

## Speech
- Easy to perform
- Poor [[Adoption#Performative interaction|social acceptability]]
- Speech recognition fails in noisy environments

## Device movement
- Unrestrictive
- Discreet
- Eyes-free

Example interactions:
- Twisting a ring
- Twisting or pushing a watch face
- Nodding while wearing a device on your head

# Output modalities
## Offloading
- Output to another device
	- E.G the user's phone or headphones

## Projected screens
- Projecting a screen onto a surface such as your wrist
	- From glasses on a watch
- Use occlusion of the light to sense inputs

## Haptics
- Many signals can be represented
- Can be directional
	- E.G a map app that vibrates the left/right motor when you need to turn
