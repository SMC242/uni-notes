This is an index of terms used in [[Functional Programming]]. They like their jargon...

# Side effect
- Something that affects state outwith the function
- Side effects must be used at some point in any app, otherwise it would do nothing

## Examples
- I/O
	- Reading from STDIN
	- Writing to a file
	- Sending an HTTP query
- Mutating state
- Throwing an error

# Pure function
- A function that lacks [[#side effect]]s
- Its output depends purely on its input - it is predictable
- Ideal for [[Concurrency|concurrent programming]] because race conditions are not possible

## Examples
```python
def add(x: int, y: int):  return x + y
```

```haskell
max :: [a] -> [a]
max [] = error "Can't do max on an empty list"
max [x] = x
max xs = max' (head xs) (tail xs)
	where
		max' acc xs' = max' (if x > acc then x else acc) (tail xs')
			where x = head xs'
```

## Not examples
```python
# This sorts the list in place - it mutates the list
def bubbleSort(xs: list) -> None:
	n = len(xs)
	for i in range(n):
		for j in range(0, n - i - 1):
			if xs[j] > xs[j + 1]:
				xs[j], xs[j + 1] = xs[j + 1], xs[j]

# Async functions and threads are not pure!
# They may be used to execute pure functions
async def get(url: str):  return await aiohttp.get(url)
```

# Higher Order Function
- A function that takes another function as a parameter
- Useful for extending a function ("wrapping") or deferring implementations

## Examples
```python
from functools import wraps

# The decorator pattern in Python uses higher order functions
def with_logging(func):
	@wraps(func)  # Preserving the metadata of `func`
	def inner(*args, **kwargs):
		print(f"Calling {func} with {args=} and {kwargs=}")
		result = func(*args, **kwargs)
		print(f"The result of {func} was {result}")
		return result
	return inner

@with_logging
def add(x: int, y: int) -> int:  return x + y
```

```haskell
-- `foldr` applies the `+` function to each element in the list
sum :: [Int] -> Int
sum xs = foldr (+) 0 xs
```

```typescript
// React represents components as functions.
// It is common to wrap these components in other components.
// It is possible to take an arbitrary component
import { ComponentType, FC } from "react";

interface ItemListProps {
	items: string[];
}

const ItemList: FC<ItemListProps> = ({items}) => (
	<ul>
		{items.map((e, i)) => <li key={i}>{e}</li>}
	</ul>
)

const WelcomeUsers = ({users, List}: {users: User[], List: ComponentType<ItemListProps>}) => {
	return (
		<>
			<h1>Welcome to all our users:</h1>
			<List items={users.map(u => u.name)}/>
		</>
	)
}

// Can be used like
<WelcomeUsers users={users} List={ItemList} />
```