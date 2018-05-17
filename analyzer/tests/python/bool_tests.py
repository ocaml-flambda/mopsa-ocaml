import mopsa

def test_false():
    mopsa.assert_equal(+False, 0)
    mopsa.assert_equal(-False, 0)

def test_true():
    mopsa.assert_equal(+True, 1)
    mopsa.assert_equal(-True, -1)
