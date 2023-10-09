# Overview
This notation system is used when talking about algorithms relating to strings

# String
$$
\begin{align*}
s &= s_{0}s_{1} \dots s_{m-1}\\
\textrm{where}\\
m &= \textrm{the length}\\
s[i] &= \textrm{the }(i + 1) \textrm{ element}\\
s[i..j] &= \textrm{a substring from } i \textrm{ to } j
\end{align*}
$$

# Prefixes
The $j$th prefix is the first $j$ characters in $s$:
$$s[0..j - 1]$$

# Suffixes
The $j$th suffix is the last $j$ characters in $s$: 
$$s[m - j..m - 1]$$