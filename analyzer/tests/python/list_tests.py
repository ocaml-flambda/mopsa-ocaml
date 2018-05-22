import mopsa

def test_initialization_with_same_values():
    l = [1, 1]
    x = l[0]
    mopsa.assert_equal(x, 1)

def test_len_of_initialization():
    l = [1, 1]
    x = len(l)
    mopsa.assert_equal(x, 2)

def test_add():
    l1 = [1, 2, 3]
    l2 = l1 + [4, 5]
    n = len(l2)
    mopsa.assert_equal(n, 5)
