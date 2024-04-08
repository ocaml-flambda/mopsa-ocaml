from counter import Counter
import random

c = Counter()
p = random.randrange(128)
print(f"p = {p}")
c.incr(2**p-1)
c.incr()
r = c.counter
print(f"r = {r}")
