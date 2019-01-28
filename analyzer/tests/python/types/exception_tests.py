import mopsa

def test_catching_raised_exception():
    x = 3.14
    try:
        x = "a"
        raise Exception
    except Exception:
        x = 3
    mopsa.assert_safe()
    mopsa.massert(isinstance(x, int))

def test_catching_superclass_exception():
    x = 0
    y = None
    mopsa.massert(not(isinstance(y, int)))
    try:
        if isinstance(x, int):
            raise ZeroDivisionError
        else:
            y = 10/x
    except Exception:
        y = 0
    mopsa.assert_safe()
    mopsa.massert(isinstance(y, int))

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
    mopsa.massert(isinstance(y, int))

def test_incorrect_raise():
    raise 2
    mopsa.assert_exception(TypeError)
