# Overview
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
	- Hard to move