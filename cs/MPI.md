---
tags:
  - DistributedSystems
---
# Overview
An API for [[Message Passing]]

# Semantics
- Synchronous messaging: both the sender and receiver block
	- Deadlocks can occur if you send without receiving ETC
- Messages are ordered and reliable (messages will be received in order and never lost)
	- Similar to [[TCP]], sometimes implemented with TCP`
- *Process ranks* are IDs for the processes
	- You need to know the rank to send a message to or receive from it 
- Typically point-to-point: you send one message and receive one
- Uses specific low-level types - you must explicitly tell MPI what type you're sending
	- Some language bindings will abstract this from you (E.G C++ templates will know that you're sending a float)
- Non-blocking mode is available in recent versions with `MPI_Wait`, `MPI_Test`
- Thread-safe by default but [[multithreading]] is difficult to combine with it
	- Typically you use MPI + [[OpenMP]]

# MPI
- The Message Passing Interface is a specification for message passing, like [[POSIX threads|pthreads]] is for [[multithreading]]

## Compiling
- A wrapper around GCC is used
	-  But even interpreted languages can have MPI bindings!
- Not a plugin like [[OpenMP]]

```bash
mpicc mpi_prog.c -o mpi_prog
```

## Running
`mpirun` and `mpiexec` are used to run MPI binaries
```bash
mpirun -n 10 ./mpi_prog
```

Or you can specify host names
```bash
mpirun--hosts gpgnode-01,gpgnode-02 ./mpi_prog
```

# Role pattern
- You use if statements to create roles based on the rank the process has

```c
// The leader node or "root"
if (proc_id == 0) {
	// Send work to other nodes
}
else if (proc_id == 1) {
	// Process work
}
else if (proc_id == 2) {
	// Process a work with a different tag
}
else {
	// Other workers
}
```

## API

### Communicators
- A group that processes join

```c
MPI_Init(&argc, &argv);
int nump, id;
// Get the count of all processes available and put it in nump
MPI_Comm_size(MPI_COMM_WORLD, &nump);
// Get the rank of this process
MPI_Comm_rank(MPI_COMM_WORLD, &id);
// ...
MPI_Finalize();  // Shutdown
```

### Sending messages
```c
MPI_Send(
 const void* buf,
 int count,
 MPI_Datatype datatype,
 // The rank of the process to send to
 int dest,
 // For specifying custom message types (E.G errors, data, instructions)
 int tag,
 MPI_Comm comm
 )
```

### Receiving
```c
MPI_Recv(
 void* buf,
 int count,
 MPI_Datatype datatype,
 // The rank of the process to receive from
 int source,
 // The type of message you want to receive.
 // Will only receive messages with this tag
 int tag,
 MPI_Comm comm
 // For errors and metadata like source
 MPI_Status *status
 )
```

### MPI_Datatype
- Language-neutral data size descriptors
	- Because binaries written in different languages can communicate with each other
	- Languages might disagree on word order, struct field arrangement, data type sizes (E.G u32 for ints in one language, u64 for others)

### Collective operations
- Operations that involve multiple processes
- Blocks until every process reaches the instruction
	- Can deadlock if a process in the group never reaches it

- Broadcast: send to all processes
	- `int root` parameter: the rank of the process that has the data to send
		- Should be the same on all call sites
	- Used for sharing data across all notes or shutdown signals
	- Sometimes it's cheaper to recompute than send a large amount of data
	```c
	int MPI_Bcast(
		 void *buffer,
		 int count,
		 MPI_Datatype datatype,
		 int root,
	     MPI_Comm comm 
   )	
	```
- Scatter: distribute messages across processes
	- Send parts of `void* sendbuf` to distribute`int sendcount` chunks to each process
	- Each process will receive the same number of chunks => the length of `sendbuf` must divide by `sendcount` evenly
```c
	int MPI_Scatter(
		const void *sendbuf,
		int sendcount,
		MPI_Datatype sendtype,
	    void *recvbuf,
		int recvcount,
		MPI_Datatype recvtype,
		int root,
	    MPI_Comm comm
    )
```
- Gather: collect messages from processes
	- The messages will be ordered by rank
	```c
		int MPI_Gather(
			const void *sendbuf,
			int sendcount,
			MPI_Datatype sendtype,
			void *recvbuf,
			int recvcount,
			MPI_Datatype recvtype,
			int root,
			MPI_Comm comm
		 )	
	```
- Reduce: aggregate with a function
	- Uses a built-in list of reduction operations of type `MPI_Op` (E.G add, subtract, and, or)
```c
	int MPI_Reduce(
		const void *sendbuf,
		void *recvbuf,
		int count,
	    MPI_Datatype datatype,
		MPI_Op op,
		int root,
		MPI_Comm comm
	 )
```


# Pros
- Flexible and thread-safe: you can mix in multi-threading
- You can build your own abstractions on top of it - MPI only specifies communication
- Extremely performant: scales to thousands
- Complex: easy to deadlock due to blocking communication
- Verbose: the coordination code obfuscates the actual logic
	- You need to know specific ranks

# See also
- [[Concurrency In Go|Goroutines]]: Go has a similar model (but it is not MPI)