import mopsa

def test_types():
    a = float()
    b = float(1)
    c = float("123")
    d = float(1.0)

    e = int()
    f = int(1)
    g = int(1.5)
    h = int("123")
    i = int("123", 2)

    x = input()
    y = input("what's your name?")

    mopsa.assert_safe()
    mopsa.massert(isinstance(a, float))
    mopsa.massert(isinstance(b, float))
    mopsa.massert(isinstance(c, float))
    mopsa.massert(isinstance(d, float))

    mopsa.massert(isinstance(e, int))
    mopsa.massert(isinstance(f, int))
    mopsa.massert(isinstance(g, int))
    mopsa.massert(isinstance(h, int))
    mopsa.massert(isinstance(i, int))

    mopsa.massert(isinstance(x, str))
    mopsa.massert(isinstance(y, str))

# a := float
# b := float
# c := float
# d := float
# e := int
# f := int
# g := int
# h := int
# i := int
# x := str
# y := str
