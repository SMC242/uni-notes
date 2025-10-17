---
tags:
  - HCI/HealthTech
---
# Overview
Assuming that digital systems are always correct can lead to safety concerns due to bugs

- Not all systems in use in healthcare are designed for it
	- iPhone calculator apps
- Devices can be certified for medical use by the MHRA
	- A "conformity mark" will be put on the device or software
	 - The device must be registered and monitored for issues

# Swiss cheese model of hazards
- Practice defence in depth
	- Assume each layer has gaps and make sure the next layer can catch errors
	- A hazard becomes a loss when multiple layers fail
- Two types of failures:
	- Active: a human making a mistake
	- Latent conditions: fixable design problems
- Don't blame medical professionals: everyone make mistakes, especially in the stressful environment of a hospital (understaffed, long shifts)
	- Catch mistakes
	- Blame encourages people to hide mistakes rather than improving systems
	- Design systems that offload as much responsibility from nurses as possible

![Image of Swiss cheese model](https://www.damotech.com/hs-fs/hubfs/Swiss_Cheese_Safety_Model.jpg?width=600&height=377&name=Swiss_Cheese_Safety_Model.jpg)

> [!EXAMPLE] Keypads
> When setting IV drip dosages, don't let the nurse key in the dosage number. Let them increment the digits individually so that they can't enter an extra zero