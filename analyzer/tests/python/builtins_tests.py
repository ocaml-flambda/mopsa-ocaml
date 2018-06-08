import mopsa

def test_define_builtin():
    def int(x): return x + 1
    y = int(2)
    mopsa.assert_equal(y, 3)

def test_define_builtin_in_one_branch():
    if mopsa.random_bool():
        def int(x): return x + 1
    else:
        pass
    y = int(2)
    mopsa.assert_exists(y == 2)
    mopsa.assert_exists(y == 3)
