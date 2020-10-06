import mopsa

def test_len():
    s = "hello"
    l = len(s)
    mopsa.assert_equal(l, 5)

def test_add():
    s1 = "hello"
    s2 = " world"
    mopsa.assert_equal(s1 + s2, "hello world")

def test_mul():
    s = "a" * 5
    mopsa.assert_equal(s, "aaaaa")
