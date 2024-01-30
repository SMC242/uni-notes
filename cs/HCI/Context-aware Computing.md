---
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
- Tell the user what the system thinks is happening so they know why it's making its decisions