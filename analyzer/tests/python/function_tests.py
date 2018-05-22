import mopsa

def test_default_argument():
    def f(x, y):
        return x + y

    def g(x, y, z = 5):
        return x + y + z

    res = f(1, 2) + g(3, 4) + g(6, 7, 8) # 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8

    mopsa.assert_equal(res, 36)

def test_global():
    def incr(x):
        global i
        i = i + x
    i = 1
    incr(10)
    mopsa.assert_equal(i, 11)

def test_arg_mismatch():
    def f(x):
        return x
    y = f(1, 2)
    mopsa.assert_exception(TypeError)
