import mopsa

def test_init():
    class C:
        def __init__(self):
            self.x = 10
    c = C()
    mopsa.assert_equal(c.x, 10)
