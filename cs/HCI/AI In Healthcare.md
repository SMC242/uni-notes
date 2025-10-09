---
tags:
  - HCI/HealthTech
  - AI
---
# Overview
[[AI]] systems can be used to reduce wait times and automatically diagnose health issues

# Use cases
- Diagnosis/screening: can crunch lots of data and run preliminary checks
	- Reduces cost as there is no need to get a nurse
	- Can detect health conditions such as asthma by monitoring blood oxygen levels
		- Reduces GP wait times
- [[LLM]]s can create synthetic data to train other AI models
	- Collecting real data is expensive, time-consuming, and has privacy concerns
- Support: AI (social robots) support robots could help the elderly
	- As the population ages, recruiting support workers will become more difficult
	 - Can automatically monitor cognitive decline and remind about medicine
	- Chatbots (interactive AI) can help with anxiety, fears, and monitoring general wellbeing
- Treatment design: aggregate patient data and notes; medical records; research; and clinical expertise
	- Customising treatment plans (E.G a patient might have bladder issues so can't take spirolactone)
	- Match doctors to patients (especially important for therapy)
	- Predict risk of diseases (medical prognosis)
- Drug discovery: AI can generate and analyse drug candidates to suggest future research areas
	- Exploits AI's ability to ingest vast amounts of data quickly
	- Can aid simulations too
- Assist data analysis: highlighting key features on scans
	- Particularly useful in cardiology and radiology
- Automated/assisted surgery: robots are more accurate than human hands. Already, robots are used instead of human hands as they don't shake

# Structure of AI health tech
1. Sensing: use [[sensors]] to gather data automatically
	- E.G fitbits, phone apps, rings. See [[Wearable Devices]]
2. Processing: [[maths/Signals#Noise|de-noising]], interpolating missing values, enhancing data with aggregates or data from other sources
	- Building health profiles
	- AI decision-making
	- Finding anomalies
	- Summarising temporal data for accessibility (E.G sleep trends over time)
3. [[Machine Learning]] models: to make predictions based on the data
	- On-device models are preferable due to privacy concerns about sending private data over the internet
		- Constrains the power and generality of the models
		
	