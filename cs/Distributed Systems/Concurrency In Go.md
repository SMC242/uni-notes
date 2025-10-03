---
aliases:
  - Goroutines
tags:
  - Concurrency
---
# Overview
Go implements concurrency as a primitive with the `go` keyword. The threads spawned are called "goroutines"

- [[cs/Distributed Systems/Computational Models#Concurrency|Concurrent]] philosophy, but often runs in [[cs/Distributed Systems/Computational Models#Parallellism|parallel]]
	- Problems are broken up into parts that pass messages to each other
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
- Shared memory only
- Non-deterministic
	- Messages can be sent or received by multiple senders or consumers
	- Messages are received in a random order when `select` is used

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

## Advantages
- No explicit locking
- Implicit ownership transfer via the channels (you get the data and now it's yours)
- Abstracts away the communication
