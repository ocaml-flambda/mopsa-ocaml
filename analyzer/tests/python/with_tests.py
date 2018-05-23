import mopsa

def test_enter_and_exit_are_called():
    g = 0
    class C:
        def __enter__(self):
            global g
            g = g + 1
            return self

        def __exit__(self, type, value, traceback):
            global g
            g = g + 1
            pass

    with C() as c:
        g = g + 1
        mopsa.assert_equal(c.x, 1)

    mopsa.assert_equal(g, 3)

def test_enter_and_exit_are_called_after_exception():
    g = 0
    class C:
        def __enter__(self):
            global g
            g = g + 1
            return self

        def __exit__(self, type, value, traceback):
            global g
            g = g + 1

    with C() as c:
        g = g + 1
        raise Exception

    mopsa.assert_safe()
    mopsa.assert_equal(g, 3)

def test_exit_reraise_exception():
    class C:
        def __enter__(self):
            return self

        def __exit__(self, type, value, traceback):
            return False

    with C() as c:
        raise Exception

    mopsa.assert_exception(Exception)
