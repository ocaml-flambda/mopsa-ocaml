import mopsa

def test_add():
    x = 1.2
    y = 10.4
    mopsa.assert_equal(x + y, 11.6)

def test_multiply():
    x = 4.2
    y = 2.0
    mopsa.assert_equal(x * y, 8.4)
