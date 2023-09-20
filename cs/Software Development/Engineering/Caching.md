# Overview
- Store recently-used values in a place that is quick to access
- Used to prevent recomputing values that are slow to compute or fetch

## Examples
- CPUs have 3 levels of caches
	- Each level is slower but larger than the last
	- Stores the data located at frequently-accessed memory locations
- Functions may be memoised
	- The most common implementation of this is using a hashmap of `hash(arguments): output`
	- Can only be used on [[Functional Programming Terms#Pure Functions|pure functions]]
	- See [[#Memoisation Example]]
- Browsers cache query results

# Why is this faster?
- Caching is used when checking if something is in the cache is faster than computing it
- If it is in the cache, we call it a "cache hit"
	- This is almost instant
- If not, it is a "cache miss"
	- Worst case
	- Missing the cache only adds a small amount of time to the execution time
- Unless inputs to a function have an extreme degree of variance, caching will make the function faster

# Types
- These types of cache are bounded, meaning that they have a max size
- When the cache is full, it flushes some values based on some condition

## FIFO
"First-In-First-Out"
- Implemented with a queue
- Removes the oldest value first (the head of the queue)
- New values are appended to the queue

## LRU
"Least Recently Used"
- Remove the value that hasn't been used for the longest period of time
- Each time a value is added, bump it to to the head of the cache

## LFU
"Least Frequently Used" or "Non-frequently Used"
- Count how many times each value has been accessed
- Remove the value with the lowest counter

# Memoisation example
Here is an implementation in Python. Note that it does not flush values, so it would take up infinite memory as time goes on
```python
from functools import wraps
from typing import TypeVar, Callable

R = TypeVar("R")

# NOTE: does not take kwargs into account because dicts are a pain to hash
def memoise(func: Callable[..., R]):
    cache: dict[int, R] = {}
    @wraps(func)  # Preserve metadata of `func`
    def inner(*args, **kwargs):
        hashed = hash(args)
        if hashed in cache:
            return cache[hashed]
        result = func(*args, **kwargs)
        cache[hashed] = result
        return result

    return inner

@memoise
def factorial(n: int) -> int:
	return n * factorial(n - 1) if n > 0 else n
```

> [!warning]
> *Never* ask a Haskell programmer how to memoise a function. They will send you academic papers

# Functools API
- `functools` is a library of [[Functional Programming Terms#Higher Order Functions|higher order functions]] in Python
- It provides the `lru_cache` decorator