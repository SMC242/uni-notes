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

