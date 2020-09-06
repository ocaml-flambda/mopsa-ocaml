import mopsa

def test_types():
    x = [1, 2, 3]
    mopsa.assert_safe()
    if not x["string"]:
        pass
    mopsa.assert_exception(TypeError)

# unsat
