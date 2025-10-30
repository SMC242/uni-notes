---
tags:
  - Dissertation
---
# Overview
Ideas for research questions for my [[Docker Vulnerabilities Map|Docker Vulnerabilities disseration]]

# Research questions
## Efficiency vs effectiveness

Which DAST toolchain has the best trade-off between speed and vulnerabilities uncovered?

- [[Between vs Within Designs#Within subjects|Within-subjects]] design

## Best combination of tools

Which two tools together uncover the most vulnerabilities?

- Comparison to: one single tool (Docker Scout)
- Could use for [[Dynamic Application Security Testing|DAST]] or [[Static Application Security Testing|SAST]]

1. What tools are available? 
2. Which ones fit together to produce something better?
3. Can I build tool that bridges the gaps between two tools?
	- Using Dive and Trufflehog

## Are native tools better?

Does [[Docker SAST Tools|Docker Scout]] outperform other [[Docker SAST Tools|SAST tools]]?

- Scout is developed in-house so may have access to expertise that third-party tools don't

# Objectives
1. Measure the *coverage* for each DAST tool in lab conditions
2. Quantify the trade-off between the coverage of DAST tools and the cost (computational overhead and processing time) of running them
	2.1. Are there type of apps that are particularly time-intensive to scan?
3. Identify real-world effectiveness of each tool using samples of images from DockerHub
4. Evaluate the usefulness of vulnerability warnings from each tool (*rate of exploitable warnings*)

## Managing bias

- All of these factors in sample selection will limit the generalisability (by biasing the data)
	- Date
	- Popularity
	- Type of server (E.G ASWGI)
	- Source: DockerHub isn't the universe
- Don't assume: you have to back everything up

> [!NOTE] Assumption example
> Closed source software isn't inherently different from open source --> still the same vulnerabilities present. Yet we don't know that for a fact because nobody has proved that. Without proving it, you can't hold it to be true

- Define real-world slice
- Label lab images
- Select and justify tool choice

## Metrics

- Coverage: percentage of vulnerabilities discovered in a set of known-vulnerable containers
- Execution time: how long the tool takes to execute on average
	- Plot this as a box-and-whisker as the range is important. Certain apps will be more expensive to scan and discovering those types may be important
	- Being slow for a particular app type would limit usability for those apps
- Real-world effectiveness: number of vulnerabilities discovered in images from DockerHub
	- This would be a subset of web applications as most DAST tools available are for websites
	- The 
- Rate of exploitable warnings: Percentage of CVEs found in the [KEV](https://www.cisa.gov/known-exploited-vulnerabilities-catalog)
	- The KEV is a database of vulnerabilities that are actively exploited
		- It's maintained by the American government, so they may have an interest in not including vulnerabilities that their agencies use on other countries
	- Could be enhanced with reachability analysis but

### Evaluating relevance

- Real-world usefulness: percentage of real-world vulnerabilities that are manually classified as impactful
	- Impact: based on CVE rating and relevance to the app in question
		- Relevance would be scored from 1 to 5 with the following criteria:
	- $for \ relevance \in {1..5}, CVE\ score \in {0.1..10} : impact = CVE\ score \times relevance$
	- I will normalise the impact to the range $0..1$ so that it will be on the same scale as coverage and therefore can be plotted on the same graph: $impact' = \frac{CVE\ score \times relevance - 0.1}{50 - 0.1}$

> [!WARNING] Not using this metric
> I decided this was too hard to quantify because I would need to audit the codebase of each app to determine whether a vulnerability is exploitable. Also ranking exploitability is difficult and would amount to guessing in most cases
> 
See https://www.indusface.com/learning/exploitable-vs-non-exploitable-vulnerabilities/