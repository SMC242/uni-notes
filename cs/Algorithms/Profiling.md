# Overview
Testing a program on a large dataset to see how it performs and which functions are bottle-necking it

# Timing
You may want to see how long a function takes. This is done by recording the time before and after running the function

```python
from time import time

start = time()
f(x)
print(time() - start)
```

```javascript
console.time();
f(x);
console.timeEnd();
```