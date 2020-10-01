import mopsa

def test_single_yield():
    def gen():
        yield 1

    g = gen()
    x = next(g)
    mopsa.assert_equal(x, 1)

def test_two_yields():
    def gen():
        yield 1
        yield 2
    g = gen()
    x = next(g)
    x = next(g)
    mopsa.assert_equal(x, 2)

def test_argument():
    def gen(x):
        x = x + 1
        yield x
        x = x + 1
        yield x
    g = gen(10)
    x = next(g)
    x = next(g)
    mopsa.assert_equal(x, 12)

def test_access_outside_scope():
    a = 1
    def gen(x):
        x = x + a
        yield x
        x = x + a
        yield x
    g = gen(0)
    x = next(g)
    a = a + 1
    x = next(g)
    mopsa.assert_equal(x, 3)

def test_stop_iteration_when_terminated():
    def gen():
        yield 1
    g = gen()
    x = next(g)
    mopsa.assert_safe()
    x = next(g)
    mopsa.assert_exception(StopIteration)

def test_auto_stop_iteration_when_terminated():
    def gen():
        yield 1
    g = gen()
    try:
        x = next(g)
        x = next(g)
    except:
        pass
    mopsa.assert_safe()
    x = next(g)
    mopsa.assert_exception(StopIteration)

def test_loop():
    def gen():
        i = 0
        while i < 10:
            yield i
            i = i + 1
    g = gen()
    for x in g:
        pass
    mopsa.assert_safe()
    # FIXME: precision on x...
