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

# Impact of change
- Changes propagate very quickly with digital tech
	- Instantly affects millions of people
- There must be a non-digital alternative to your app
	- People who can't operate digital devices or don't have access to one are cut off from healthcare

# Ethical lag
![[Ethical Lag]]

- Can be mitigated with regular talks with [[Stakeholders]]
- The NHS makes ethical approval very slow and labour-intensive to slow down innovation so that regulation can move ahead
	- You can't access patients without ethics approval
- Workaround: work with a charity to access patients for focus groups
	- You can't apply treatments though

# Sensitive data
- The following data has more stringent regulations around it:
	- Health
	- Genetic
	- Biometrics
	- Sexual activity
- Higher penalties if lost in a data breach
	- Things that could be used for exploitation, fraud, or targeted violence
	- Long-lasting consequences because they're immutable characteristics (I.E your DNA won't change but your account password can)
- You must justify when you're storing medical data off-device and show how you're securing it
	- You must also manage access within your company to avoid an insider leaks it
	- Keep data up-to-date