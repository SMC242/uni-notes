---
aliases:
  - pthreads
tags:
  - Multithreading
  - Parallelism
---
# Overview
A standard interface for [[Multithreading|threads]] specified by [[POSIX]]
- Low-level interface
- Often used in C, but not limited to it

# Spawning threads
```c
#include <pthread.h>

void* func(void*) {
	// Do stuff
}

pthread_t thread;
// The first NULL is for a config struct
if (pthread_create(&thread, NULL, func, NULL) != 0) {
	// Handle failure to spawn thread
}

struct args {
	int arg1;
	int arg2;
}

void* func2(void *arguments) {
	// Cast void pointer back to struct
	struct args *args = arguments;
	// Do stuff with arguments
}

pthread_t thread2;
struct args arguments;
arguments.arg1 = 1;
arguments.arg2 = 2;
pthread_create(&thread2, NULL, func2, (void *)&args);
```

See [the manpage](https://man7.org/linux/man-pages/man3/pthread_create.3.html)

# Joining threads

```c
pthread_t thread;
// Spawn the thread
// ...
// The return value from the thread will be put here
void* output;
pthread_join(thread, &output);
```

See [the manpage](https://man7.org/linux/man-pages/man3/pthread_join.3.html)

# Managing mutexes

```c
// Should be declared in the global scope that both
// the function and the main function can access it
static pthread_mutex_t mutex;
// The NULL is a config
pthread_mutex_init(&mutex, NULL);  
// Do stuff with threads
pthread_mutex_lock(&mutex);
pthread_mutex_unlock(&mutex);
pthread__mutex_destroy(&mutex);
```

See the manpages:
- [`pthread_mutex_init`](https://pubs.opengroup.org/onlinepubs/7908799/xsh/pthread_mutex_init.html)
- [`pthread_mutex_lock`](https://pubs.opengroup.org/onlinepubs/7908799/xsh/pthread_mutex_lock.html)
- [`pthread_mutex_unlock`](https://www.ibm.com/docs/en/zos/2.5.0?topic=functions-pthread-mutex-unlock-unlock-mutex-object)
- [`pthread_mutex_destroy`](https://pubs.opengroup.org/onlinepubs/007904875/functions/pthread_mutex_destroy.html)

# Condition variabbles

```c
static pthread_mutex_t mutex;
static pthread_cond_t condvar;

// The NULL is a config
pthread_cond_init(&condvar, NULL);

pthread_mutex_lock(&mutex);
// Wait until the condition becomes false
while (x != y) {
	// Block until a signal is received
	pthread_cond_wait(&condvar, &mutex);
}
pthread_mutex_unlock(&mutex);

pthread_mutex_lock(&mutex);
// Do something
pthread_mutex_unlock(&mutext);
// Will wake up threads that are waiting
pthread_cond_signal(&condvar);

pthread_cond_destroy(&condvar);
```

# See also
- [The pthreads manpage](https://man7.org/linux/man-pages/man7/pthreads.7.html)