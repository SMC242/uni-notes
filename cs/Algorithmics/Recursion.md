# Definition
- A function is recursive if it calls itself

```haskell
factorial :: (Eq a, Num a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n - 1)
```

`factorial 3` produces the following call chain:
```
factorial 3 = 3 * (factorial 2)
factorial 3 = 3 * (2 * (factorial 1))
factorial 3 = 3 * (2 * (1 * 1))             <-- base case reached
```

# Anatomy of a recursive function
## Base case
- This is the point where the function stops recurring and the call chain starts to collapse
- In the `factorial` example, `n = 1` is the base case

## Reduction
- The dataset must get smaller each time the function recurs
- In this example, `n` reduces by `1` each time the function recurs

# Types
## Linear
- Each recursive call produces at most 1 other call

## Binary
- Each recursive call produces 0 or 2 more calls

## Ternary
- Each recursive call produces 0 or 3 more calls

# Visualising recursive functions
## Recursion trace diagram
A recursion trace diagram can be used to show each step

![Recursion trace](https://d2vlcm61l7u1fs.cloudfront.net/media%2F752%2F7522d750-706b-4a02-aa6e-43acaafbdaff%2FphpM2PaE8.png)

## Recursion tree
- Representing the [[#Recursion trace diagram|trace]] of a recursive function is useful for [[#Binary|n-ary]] recursive functions

![Fibonnaci recursion tree](https://miro.medium.com/v2/resize:fit:925/1*svQ784qk1hvBE3iz7VGGgQ.jpeg)

### Analysing trees
- The height of a tree is the maximum number of edges from the root node to any node
	- I.E the number of layers
- The number of nodes in a binary tree is at most $2^{h + 1} - 1$ where $h$ is the height of the tree
- The Fibonacci function above is $O(2^n)$ because it is a binary recursive function of height $n$
	- Exponential complexity

# The cost of recursion
- Every time a function recurs, the local variables must be saved in a stack frame
- This is costly in terms of memory
- Having too many calls on the stack causes a stack overflow exception
	- This is when the portion of memory allocated to the call stack becomes full
	- There is no more space for another stack, so the program crashes
- A maximum number of recurrences (recursion limit) is enforced by some programming languages to stop recursion before a stack overflow occurs

# Tail recursion
- A linearly recursive function where the recursive call is the final operation
- Compilers can easily optimise these

## Examples
```python
def swap(xs: Sequence[T], i: int, j: int) -> None:
	xs[i], xs[j] = xs[j], xs[i]

def reverse(xs: Sequence[T], i: int, j: int) -> None:
	if i >= j:  return        # Base case
	swap(xs, i, j)
	reverse(xs, i + 1, j -1)  # The last operation
```

```haskell
factorial :: (Eq a, Num a) => a -> a  
factorial n = aux n 1  
where  
 aux x acc | 0         = acc  
           | otherwise = aux (n - 1) (n * acc)
```

## Counter-example
- The factorial example is not tail recursive
- This is because `(factorial n - 1)`  must be calculated before the multiplication can happen

# Conversion to iterative
Iterative algorithms are:
- More memory-efficient
- Are faster due to:
	- Less overhead from creating stack frames (this cost depends on the language)
	- Most languages are better optimising iterative loops

## Process
- The base case becomes the condition for a while loop
	- The base case may need to be inverted if you are using the guard pattern (E.G `if i >= j:  return`)

```python
def reverse(xs: Sequence[T]) -> Sequence[T]:
	i, j = 0, len(xs) - 1
	while i < j:
		swap(xs, i, j)
		i += 1
		j += 1
	return xs
```

# Best practices
## Hide internal arguments
- If you have extra arguments that should not be inputted by the user, hide them with an auxiliary function

Here is the `reverse` function from earlier rewritten this way:
```python
def reverse(xs: Sequence[T]) -> None:
	# Using a closure to avoid exposing `i` and `j` to the user
	def aux(i: int, j: int) -> None:
		if i >= j:  return      # Base case
		swap(xs, i, j)
		inner(i + 1, j -1)      # The last operation
	inner(0, len(xs) - 1)       # Provide starting values
```

This is more difficult in Java
```java
public class Reverser {
	public static void reverse(xs: List<?>) {
		_reverse(xs, 0, xs.size() - 1);
	}

	private static void _reverse(xs: List<?>, i: int, j: int)
	{
		if i >= j:  return;
		swap(xs, i, j);
		_reverse(xs, i + 1, j - 1);
	}

	private static <T> void swap(xs: List<T>, i: int, j: int) {
		T temp = xs.get(i);
		xs[i] = xs[j];
		xs[j] = temp;
	}
}
```

## Return result
- Regardless of whether you are changing the input in place, it's nice to return the result when possible
- This isn't always convenient
	- E.G [[Merge Sort]]
	- It is very easy when the function is just an expression such as:
```javascript
factorial = n => n == 1 ? 1 : n * factorial(n - 1)
```

Here is `reverse` rewritten to return the result:
```python
def reverse(xs: Sequence[T]) -> None:
	def inner(i: int, j: int) -> None:
		...
	inner(0, len(xs) - 1)
	return xs
```

- If mutating the input, document this to warn users to not reuse the original reference
	- Alternatively, copy the input

This is to avoid bugs such as:
```python
original = [1, 4]
new = reverse(original)
print(new)               # [4, 1]
print(original)          # Expected: [1, 4], actual: [4, 1]
```