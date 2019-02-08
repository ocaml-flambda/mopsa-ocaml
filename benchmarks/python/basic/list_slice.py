"""
The Computer Language Benchmarks Game
http://benchmarksgame.alioth.debian.org/

Contributed by Sokolov Yura, modified by Tupteq.
"""

n = 9
m = n - 1
perm1 = list(range(n))
k = 0
while 1:
    perm = perm1[:]
    flips_count = 0
    k = perm[0]
    while k:
        perm[:k + 1] = perm[k::-1]
        flips_count = flips_count + 1
        k = perm[0]
    if perm[0]:
        break
