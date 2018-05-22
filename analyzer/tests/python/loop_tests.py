import mopsa

def test_while():
    i = 0
    while i < 10:
        i = i + 1
    mopsa.assert_equal(i, 10)
