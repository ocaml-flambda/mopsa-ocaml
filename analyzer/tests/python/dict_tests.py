import mopsa

def test_precision_with_one_value():
    d = {"a": 1, "b": 1}
    mopsa.assert_equal(d["a"], 1)

def test_precision_with_empty_dict():
    d = {}
    d[0] = 1
    mopsa.assert_equal(d[0], 1)
