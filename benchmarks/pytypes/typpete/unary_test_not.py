import mopsa

def test_main():
    x = [1, 2, 3]
    mopsa.assert_safe()
    if not x["string"]:
        pass
    mopsa.assert_exception(TypeError)

# unsat
