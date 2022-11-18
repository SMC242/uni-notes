# Definition
$gcd(a, b)$ ("greatest common divisor") is the largest integer that divides both $a$ and $b$

# Use cases
## Filling areas
Finding the most efficient way to fill an area

Example:
Find the largest tile that could be used to fill a 152x57 square
$gcd(152, 57) = 19$

# Euclidean algorithm
![[Division#Algorithm]]

Using this formula, $gcd(a, d) = gcd(d, r)$

## Recursive definition
```haskell
gcd :: (Integral a) => a -> a -> a
gcd a 0 = a
gcd a b = gcd b ( a `mod` b)
```