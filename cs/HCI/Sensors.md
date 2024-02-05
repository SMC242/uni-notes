---
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
- 