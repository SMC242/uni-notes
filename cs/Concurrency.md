---
tags:
  - DistributedSystems
  - Concurrency
---
# Overview
- Jobs are run concurrently
	- Interactions between actors
- Uses an event loop or threads
	- Event loops interleave tasks
- Useful for I/O bound applications

![Concurrency vs parallelism](https://s3-wp-product.s3.amazonaws.com/wp-content/uploads/20240308182308/1_5P4uAgYGrsl4Lq-4ASitEQ.png)

# Metrics
- Requests per second
- Service-level agreements (SLAs)
	- Uptime
	- Response latency
	- Quality of service
- Delivery requirements: business requirements
	- E.G the system has to handle 30 billion messages per day
- Mobility: the ability to be fault-tolerant and move microservices between clouds