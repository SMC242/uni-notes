---
tags:
  - OSIModel
---

# Roles
- This [[Protocol Layering|protocol layer]] handles transferring the actual bits of data
- Masks the complexity of encoding and decoding sequences of bits

# Wired media
## Unshielded twisted pair (UTP)
- Two wires twisted together
- Encodes the data by varying the voltage
- Each wire only transmits in one direction (signal and ground)
- More twists = less noise and interference
- Capable of long distance transmission, but slowly
	- Fast at <= 100 metres
	- More noise as length increases
- Ethernet cables and telephone lines use this

## Optical fibre
- Glass core protected with a plastic jacket
- Encodes the data by changing the light intensity
- Fragile
- Unidirectional
- Little noise
- High capacity over distance
- Cheap
- Expensive to operate (ISPs probably foot this cost)

See [[Encodings]] for more information on how data is encoded in these media

# Modulation
- Shifting the frequency, amplitude, or phase of a signal to allow multiple signals through the same channel without overlapping
- Used by wireless links mainly, but can be used by wired links

# Spread spectrum communication
When only one frequency is used, interference can cause issues. Instead, frequencies are changed pseudo-randomly. The function that determines the frequency is predictable - using a seed that is shared between the sender and receiver

# Bandwith
The range of frequencies supported by the medium

### Maximum transmission rate formula
$$R_{max}=2Hlog_2V$$
where
	$R_{max}$ = max transmission rate (bits/s)
	$H$ = bandwidth
	$V$ = number of discrete values per symbol

# Noise
Noise is interference of the signal due to environmental conditions

### Signal-noise ratio
Also known as S/R or SNR
$$dB = 10log_{10} \frac{S}{N}$$
where
	$dB$ = ratio in decibels
	$S$ = signal power
	$N$ = noise floor

# Capacity
The maximum transmission rate of a channel

### Max transmission rate formula
This is Shannon's Theorem (good for them)
$$R_{max} = H log_2(1 + S/N)$$