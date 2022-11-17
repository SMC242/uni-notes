# Divides operator
$a | b$ means $a$ divides $b$ cleanly (there is no remainder)

Implications:
- $a \neq 0$
- $\exists c. (a \times c = b)$
	- $b$ can be calculated by multiplying $a$ by some factor, $c$
https://moodle.gla.ac.uk/pluginfile.php/5699780/mod_resource/content/3/af2_week4.pdf
# Chaining
You can chain the [[#Divides operator]]
$a | b$ and $b | c$ means that $a | c$ because:
$$
\begin{aligned}
& a | b \equiv a \times c = b \\
& = a | (b \times c) & \mbox{Divided by c}\\
& b | c \equiv b \times x
\end{aligned}
$$