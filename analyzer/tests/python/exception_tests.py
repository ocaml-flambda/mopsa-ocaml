import mopsa

def test_catching_raised_exception():
    x = 1
    try:
        x = 2
        raise Exception
    except Exception:
        x = 3
    mopsa.assert_safe()
    mopsa.assert_equal(x, 3)

def test_catching_superclass_exception():
    x = 0
    y = None
    try:
        if x == 0:
            raise ZeroDivisionError
        else:
            y = 10/x
    except Exception:
        y = 0
    mopsa.assert_safe()
    mopsa.assert_equal(y, 0)

def test_catching_wrong_exception():
    x = 0
    try:
        raise RuntimeError
    except ZeroDivisionError:
        y = 0
    mopsa.assert_exception(RuntimeError)

def test_default_except():
    x = 0
    y = None
    try:
        raise RuntimeError
    except:
        y = 0
    mopsa.assert_safe()
    mopsa.assert_equal(y, 0)

def test_unreachable_test():
    x = 1
    if x > 2:
        raise 2
    else:
        x = 10
    mopsa.assert_safe()
    mopsa.assert_equal(x, 10)

def test_incorrect_raise():
    raise 2
    mopsa.assert_exception(TypeError)
