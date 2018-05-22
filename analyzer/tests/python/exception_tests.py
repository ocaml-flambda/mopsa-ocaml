import mopsa

def test_raise_caught_with_same_exception():
    x = 1
    try:
        x = 2
        raise Exception
    except Exception:
        x = 3
    mopsa.assert_safe()
    mopsa.assert_equal(x, 3)

def test_unreachable_test():
    x = 1
    if x > 2:
        try:
            raise 2
        except 3:
            pass
    else:
        x = 10
    mopsa.assert_safe()
    mopsa.assert_equal(x, 10)
