# Overview
- [[Arrays|NDArrays]] have a very simple representation in memory that allows their operations to be very efficient

# Header
- The header comes before the array contents in memory
- Tells the program how to jump around the array

It contains:
- The number of dimensions
- The number of elements
- The size of each element
- The [[#shape]]
- The [[#strides]]
- A pointer to the array
- Some flags

# Shape
- The dimensions of the array
- E.G `5x4` for 5 rows with 4 columns
# Strides
- Tells you how many bytes to jump to get to the next column or row
- This can be calculated based on the array's [[#shape]] and the size of each element
- This allows jumping to rows and columns to be $O(1)$

Example:
$$
\begin{align*}
cols :&= 4\\
rows :&= 500\\
elementSize :&= 8 \textrm{ bytes}\\
\\
rowStride &= cols \times 8 = 20 \textrm{ bytes}\\
colStride &= elementSize = 8 \textrm{ bytes}
\end{align*}
$$

## Dope vectors
- Holds the striding information
- Two conventions:
	- FORTRAN-style (column-major): `[columnStrides, rowStrides]`
	- C-style (row-major): `[rowStrides, columnStrides]`
	- This defines the default order of iteration through the array

## Illife vectors
- An alternative to strides
- A nested list holding pointers to each row
- Advantage: allows [[Arrays#Definition|ragged arrays]]
- Disadvantage: less efficient
