import mopsa

def test_false():
    mopsa.assert_equal(+False, 0)
    mopsa.assert_equal(-False, 0)

def test_true():
    mopsa.assert_equal(+True, 1)
    mopsa.assert_equal(-True, -1)

def test_instanceof():
    mopsa.massert(isinstance(True, bool))
    mopsa.massert(isinstance(True, int))
    mopsa.massert(not(isinstance(True, float)))
