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

def test_closure_nonlocal():
    def f():
        x = 42
        def g():
            nonlocal x
            x += 2
            return x
        return g

    f1 = f()
    f2 = f()
    b1 = f1()
    f2()
    b2 = f2()
    mopsa.assert_equal(b1, 44)
    mopsa.assert_equal(b2, 46)

def test_closure_param():
    def mult(n):
        def m(x):
            return x * n
        return m

    x3 = mult(3)
    xa = mult('a')

    r1 = x3(1)
    r2 = xa(3)

    mopsa.assert_equal(r1, 3)
    mopsa.assert_equal(r2, 'aaa')
