import mopsa

def test_len():
    s = "hello"
    l = len(s)
    mopsa.assert_equal(l, 5)
