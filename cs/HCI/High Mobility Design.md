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
	- E.G [[Augmented Reality|AR]] glasses, heads-up displays