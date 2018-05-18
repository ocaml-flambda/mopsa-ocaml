import mopsa

def test_initialization():
    d = {"a": 1, "b": 1}
    mopsa.assert_equal(d["a"], 1)
