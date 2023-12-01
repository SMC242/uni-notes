# Overview
Non-deterministic algorithms are algorithms that use a non-deterministic subroutine. This means that their running time is unpredictable

# Requirements
- Must produce the correct answer sometimes
	- Can return false-negatives
- Cannot return false-positives

## Formal definition
- For a "yes" instance $I$ of a problem $\Pi$, there must be an execution that produces "yes"
- For a "no" instance, there must be no executions that produce "yes"

# Steps
An NDA usually consists of two main steps:
1. Make a guess
2. Verify the guess

> [!INFO] Practicality...
> These algorithms are 