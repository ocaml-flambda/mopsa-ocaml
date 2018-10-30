import mopsa

def test_false():
    mopsa.assert_equal(+False, 0)
    mopsa.assert_equal(-False, 0)

def test_true():
    mopsa.assert_equal(+True, 1)
    mopsa.assert_equal(-True, -1)

def test_instanceof():
    mopsa.assert(isinstance(True, bool))
    mopsa.assert(isinstance(True, int))
    mopsa.assert(not(isinstance(True, float)))
