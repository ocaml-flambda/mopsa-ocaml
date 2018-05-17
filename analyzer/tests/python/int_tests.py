import mopsa

def test_add():
    x = 1
    y = 2
    mopsa.assert_equal(x + y, 3)

def test_multiply():
    x = 23
    y = 10
    mopsa.assert_equal(x * y, 230)

def test_hex():
    x1 = 1
    x2 = 0x01
    x3 = 0X001
    mopsa.assert_equal(x1 + x2 + x3, 3)

def test_bool_coercion():
    i = -10
    b = True
    mopsa.assert_equal(i + b, -9)

def test_in_condition():
    i = -10
    mopsa.assert_true(i)
    mopsa.assert_false(not i)
