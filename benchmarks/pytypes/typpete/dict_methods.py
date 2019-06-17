import mopsa

def test_main():
    x = dict()
    x[2.0] = "string"
    y = x.copy()
    y.clear()
    # typpete is unsound on get: it should return str or NoneType (example
    # with x.get(3.0), which is NOT of type str)
    a = x.get(2.0)
    if a == None:
        a = ""

    d = x.pop(2.0)
    e = x.popitem()
    x.update(y)


    f = a[0] + d[0]
    g = e[0]
    h = e[1]

    mopsa.ignore_exception(KeyError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(a, str))
    mopsa.massert(isinstance(d, str))
    mopsa.massert(isinstance(e, tuple))
    mopsa.massert(isinstance(g, float))
    mopsa.massert(isinstance(h, str))
    mopsa.assert_dict_of(x, float, str)
    # TODO: I disagree with that one, this is now an empty dictionary
    # mopsa.assert_dict_of(y, float, str)
    mopsa.ignore_exception(KeyError)

# x := Dict[float, str]
# y := Dict[float, str]
# a := str
# d := str
# e := Tuple[float, str]
