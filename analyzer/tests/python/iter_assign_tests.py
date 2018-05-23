import mopsa

def test_iter_assign_on_lists():
    l = [1, 2]
    x, y = l
    mopsa.assert_exists(x == 1)
    mopsa.assert_exists(y == 2)

def test_iter_assign_on_tuples():
    t = (1, 2)
    x, y = t
    mopsa.assert_true(x == 1)
    mopsa.assert_true(y == 2)
