import mopsa

HIGHEST_PROTOCOL = mopsa.random_int()

@mopsa.unsupported
def dumps(obj, proto): pass

@mopsa.unsupported
def loads(d): pass
