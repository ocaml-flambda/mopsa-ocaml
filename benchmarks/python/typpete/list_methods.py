import mopsa

def test_types():
    x = []

    y = x.count("b")
    x.append("a")
    x.extend(["c", "d"])
    try:
        z = x.index("e")
    except ValueError:
        z = 0
    x.insert(1, "f")
    try:
        a = x.pop()
    except IndexError:
        a = ""
    try:
        x.remove("st")
    except ValueError: pass
    x.reverse()
    x.sort()
    t = a[y:z]

    class A:
        def append(self, x):
            pass

    i = A()
    i.append(1)
    mopsa.assert_safe()
    mopsa.massert(isinstance(x, list))
    mopsa.massert(isinstance(x[0], str))
    mopsa.ignore_exception(IndexError)
    mopsa.massert(isinstance(a, str))
    mopsa.massert(isinstance(t, str))
    mopsa.massert(isinstance(y, int))
    mopsa.massert(isinstance(z, int))
    mopsa.massert(isinstance(i, A))
# x := List[str]
# a := str
# y := int
# z := int
# A := Type[A]
# i := A
