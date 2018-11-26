import mopsa

def test_const():
    x = 42
    mopsa.massert(isinstance(x, int))

def test_add():
    x = 42
    y = 43
    z = x + y
    mopsa.massert(isinstance(z, int))

def test_sub():
    x = 42
    y = 43
    z = x - y
    mopsa.massert(isinstance(z, int))

def test_mult():
    x = 42
    y = 43
    z = x * y
    mopsa.massert(isinstance(z, int))

def test_div():
    x = 84
    y = 42
    z = x // y
    mopsa.massert(isinstance(z, int))

def test_comps():
    x = 42
    y = 43
    z1 = x >= y
    z2 = x <= y
    z3 = x > y
    z4 = x < y
    z5 = x == y
    z6 = x != y
    mopsa.massert(isinstance(z1, int))
    mopsa.massert(isinstance(z2, int))
    mopsa.massert(isinstance(z3, int))
    mopsa.massert(isinstance(z4, int))
    mopsa.massert(isinstance(z5, int))
    mopsa.massert(isinstance(z6, int))
