# CPU bursts
- As many programs are running at any given time, the CPU has to somehow give CPU time to all of them
- A CPU burst is giving CPU time to a program (I.E running it) for a while

# Scheduler types
## Non-preemptive 
- The process decides when it yields to the scheduler again

## Preemptive
- The scheduler decides when the process yields

# Dispatcher
- Handles switching to and running the process that the scheduler selected

Tasks:
- Context switching (loading the variables from memory)
- Switching to user mode
- Returning to the point the process was paused at

# Evaluating a scheduler
The following statistics are used to rate the performance of a scheduling algorithm:
- CPU utilisation
	- The percentage of time that the CPU is doing things
	- More is better
- Throughput
	- The rate of process completion
	- More is better
- Waiting time
	- The total time that a process spends waiting (in the READY state) for CPU time
	- Less is better
- Turnaround time
	- The time between a process being submitted to when it's finished
	- Less is better
- Response time
	- The time between submitting a process and getting its first response

# Scheduling algorithms
## FCFS
"First Come, First Served" or "First In, First Out"
- Non-preemptive

- Processes are added to a queue when they arrive
- The first process in the queue is executed
- If it needs more time, it's put at the back of the queue

<ul class="breakdown">
	<li class="pro">Easy to implement</li>
	<li class="con">High waiting time</li>
</ul>

## SJF
"Shortest Job First"
- Non-preemptive

- Uses a queue that is sorted by their remaining time
- The scheduler always picks the first process in the queue

<ul class="breakdown">
	<li class="pro">Easy to implement</li>
	<li class="pro">Low waiting time</li>
	<li class="con">Lots of small processes can starve all other processes</li>
</ul>

^6ba4e1

### SRTF
"Shortest Remaining Time First"
- [[#SJF]], but preemptive
- The difference is that the scheduler will also be called when a new process comes in
- This means that it may switch at that point
![[#^6ba4e1]]

## Non-preemptive Priority Scheduling
- Non-preemptive

- Uses a queue sorted by priority (higher priority value = lower priority)
- Takes the first process in the queue and executes it

<ul class="breakdown">
	<li class="pro">Easy to implement</li>
	<li class="con">Can starve low-priority processes of CPU time</li>
</ul>

## RR
"Round Robin"
- [[#FCFS]] but preemptive

- Each process is added to a queue when it arrives
- The head of the queue is executed for a set amount of time (the "quantum"), then bumped to the back of the queue

<ul class="breakdown">
	<li class="pro">Easy to implement</li>
	<li class="con">Can have long waiting times</li>
	<li class="con">Performance depends on the quantum</li>
</ul>