# Experimental analysis
- Implementing the algorithm and [[Profiling]] it afterwards
<ul class="breakdown">
	<li class="pro">Easy</li>
	<li class="con">You have to implement and test it first, which may be slow</li>
</ul>

# Theoretical analysis
- Work out the computations based on [[#Analysing pseudocode|pseudocode]]
- Each operation (E.G indexing an array) takes some time which we assume to be constant
<ul class="breakdown">
	<li class="pro">You don't have to implement the algorithm</li>
	<li class="pro">Fast</li>
	<li class="con">BORING, VERY BORING. Math is for nerds</li>
</ul>

# Cases
<dl>
	<dt>Best case</dt>
	<dd>The function is called with an input that makes it exit early</dd>
	<dt>Worst case</dt>
	<dd>The situation where the function runs through the entire input (E.G element not found when searching)</dd>
</dl>

# Time complexities
- $O(1)$ constant time - some data structures offer functions like this
- $O(log n)$ - binary search
- $O(n)$ linear time - the holy grail of time complexities
- $O(n \log_2 n)$ logarithmic time - most sorting algorithms
- $O(n^2)$ quadratic time - getting into the danger zone
- $O(n^x)$ polynomial time - some matrix operations are $O(n^3)$
- $O(2^n)$ exponential time - not feasible
- $O(n!)$ factorial time - not feasible

![Time complexities graphed](https://miro.medium.com/max/1200/1*5ZLci3SuR0zM_QlZOADv8Q.jpeg)

# Analysing pseudocode
- Count the operations on each line, then add them together and simplify

## Examples
- `x := arr[0]` is 2 operations: assignment and indexing
- `for i = 1 to n-1` is 2n operations: assign `i` and compare to `n - 1` for each element
- Loop bodies are usually executed `n - 1` times