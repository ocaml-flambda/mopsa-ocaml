import mopsa

def test_types():
    class A:
        def f(self, x):
            pass

    class B(A):
        def f(self, x=1):
            pass

    mopsa.assert_safe()
