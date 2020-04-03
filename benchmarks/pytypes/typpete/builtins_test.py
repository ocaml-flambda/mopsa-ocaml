import mopsa

def test_types():
    a = abs(1)
    b = bin(1)
    b = b + "2"
    c = all([1, 2])
    d = chr(1)
    d = d + ""
    e = complex()
    f = dir()
    h, i = divmod(c, 2.0)
    hh = h
    ii = i
    # j = float(b) not working in Python 3.6.7, changed into float(a)
    j = float(a)
    k = hash(j)
    l = int(j)
    m = str(l)
    n = f[l]
    mopsa.ignore_exception(IndexError)
    o = f[k]
    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(a, int))
    mopsa.massert(isinstance(b, str))
    mopsa.massert(isinstance(c, bool))
    mopsa.massert(isinstance(d, str))
    mopsa.massert(isinstance(e, complex))
    mopsa.massert(isinstance(f, list))
    mopsa.massert(isinstance(f[0], str))
    mopsa.ignore_exception(IndexError)
    mopsa.massert(isinstance(h, float))
    mopsa.massert(isinstance(i, float))
    mopsa.massert(isinstance(j, float))
    mopsa.massert(isinstance(k, int))
    mopsa.massert(isinstance(l, int))
    mopsa.massert(isinstance(m, str))
    mopsa.massert(isinstance(n, str))
    mopsa.massert(isinstance(o, str))
# b := str
# d := str
# f := List[str]
# k := int
# l := int


def test_values():
    a = abs(1)
    b = bin(1)
    b = b + "2"
    c = all([1, 2])
    d = chr(1)
    d = d + ""
    e = complex()
    f = dir()
    h, i = divmod(c, 2.0)
    hh = h
    ii = i
    # j = float(b) not working in Python 3.6.7, changed into float(a)
    j = float(a)
    k = hash(j)
    l = int(j)
    m = str(l)
    mopsa.assert_safe()
    n = f[l]
    o = f[k]
    mopsa.ignore_exception(IndexError)
    mopsa.massert(isinstance(a, int))
    mopsa.massert(isinstance(b, str))
    mopsa.massert(isinstance(c, bool))
    mopsa.massert(isinstance(d, str))
    mopsa.massert(isinstance(e, complex))
    mopsa.massert(isinstance(f, list))
    mopsa.massert(isinstance(f[0], str))
    mopsa.massert(isinstance(h, float))
    mopsa.massert(isinstance(i, float))
    mopsa.massert(isinstance(j, float))
    mopsa.massert(isinstance(k, int))
    mopsa.massert(isinstance(l, int))
    mopsa.massert(isinstance(m, str))
    mopsa.massert(isinstance(n, str))
    mopsa.massert(isinstance(o, str))
# b := str
# d := str
# f := List[str]
# k := int
# l := int
