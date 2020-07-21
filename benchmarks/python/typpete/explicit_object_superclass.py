import mopsa


def test_types():
    class A(object):
        def something(self):
            return 3

    a = A()
    b = a.something()
    mopsa.massert(isinstance(a, A))
    mopsa.massert(isinstance(b, int))
