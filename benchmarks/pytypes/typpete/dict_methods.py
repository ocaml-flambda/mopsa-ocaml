import mopsa

def test_main():
    x = dict()
    x[2.0] = "string"
    y = x.copy()
    y.clear()
    # typpete is unsound on get: it should return str or NoneType (example
    # with x.get(3.0), which is NOT of type str)
    a = x.get(2.0)
    d = x.pop(2.0)
    e = x.popitem()
    x.update(y)

    if a == None:
        a = ""
    f = a[0] + d[0]
    g = e[0]
    h = e[1]

    mopsa.assert_safe()
    mopsa.assert_dict_of(x, float, str)
    mopsa.assert_dict_of(y, float, str)
    mopsa.massert(isinstance(a, str))
    mopsa.massert(isinstance(d, str))
    mopsa.massert(isinstance(e, tuple))

# x := Dict[float, str]
# y := Dict[float, str]
# a := str
# d := str
# e := Tuple[float, str]
