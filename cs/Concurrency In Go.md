---
aliases:
  - Goroutines
tags:
  - Concurrency
  - DistributedSystems
---
# Overview
Go implements concurrency as a primitive with the `go` keyword. The threads spawned are called "goroutines"

- [[cs/Computational Models#Concurrency|Concurrent]] philosophy using threads underneath (so often executes in parallel)
	- Problems are broken up into parts that pass messages to each other
- The runtime can suspend/resume tasks running on the thread
- Message-passing system
```go
func add(x, y int) int {
	return x + y
}

func main() {
	add(1, 2)  // Synchronous call
	go add(3, 4) // Any function can be wrapped in a goroutine
}
```

## Implementation details
- Uses [[Multithreading#Green threads|green threads]]
- You can't get a value out of a goroutine
	- Unless you use [[#channels]] to communicate between goroutines
- You don't get a thread handle  --> can't kill the thread
- [[cs/Computational Models#Shared memory|Shared memory]] only

# Semantics
- Non-deterministic due to:
	- Messages can be sent or received by multiple senders or consumers
	- Messages are received in a random order when `select` is used
	- Runtime scheduling order
- You need to think in terms of [[eventual consistency]]; Don't rely on ordering
- Sending a value through a channel transfers ownership

# Channels
- How goroutines communicate
- Synchronous by default
	- Communication blocks!
- Can be passed around
- Can be closed with `close()`
	- This is why `<-` returns a result instead of just a value
- Shared memory

```go
func SendMessage(msg string, channel chan string) {
	channel <- msg
}

func ReceiveMessage(channel chan string) {
	channel := make(chan string)
	go SendMessage("hello")
	go SendMessage("Hi")
	msg1 <- channel
	msg2 <- channel
}
```

This syntax is used to receive until the channel closes:
```go
for x := range channel {
	// do stuff with the xs
}
```

## Buffered channels
- A channel that contains a bounded buffer of size `n`
	- Blocks when full

```go
bufferedChannel := make(chan int, 2)
```

## Directional channels
- Channel direction can be type-hinted
	- Not enforced by the type-checker

```go
func io(inChannel <-chan int, outChannel chan <-int) {
	// ...
}
```

## Consuming multiple channels
- The `select` construct will block until a channel is ready (randomly chosen if multiple are ready)

```go
select {
	case x <- c1: // ...
	case <- c2: // ...
}
```

## Closing channels
- Consuming channels returns an error because they can be closed with `close(chan)`
- This doesn't discard the values in the channel
	- It just signals to `select` statements to exit

```go
x, ok := <- c
if !ok {
	return
}
```
### Iterating over channels
- Syntactic sugar to avoid checking the error every time

```go
for x := range c {
	// Do stuff
}
```

## Advantages
- No explicit locking
- Implicit ownership transfer via the channels (you get the data and now it's yours)
- Abstracts away the communication

# Disadvantages
- Can deadlock if sending synchronously and nothing receives
	- Go can check for deadlocks
	- Can also livelock
- Synchronisation primitives aren't in the standard library so you can't do fine-grained locking