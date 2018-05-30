import mopsa

def sin(x): return mopsa.random_float(-1, 1)

def cos(x): return mopsa.random_float(-1, 1)

@mopsa.builtin("math.sqrt")
def sqrt(x): pass
