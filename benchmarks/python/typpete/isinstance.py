import mopsa

def test_types():
    class A:
        def f(self):
            return 1


    class B:
        def g(self):
            return 1



    def f(x):
        if isinstance(x, A):
            res = x.f()
        elif isinstance(x, B):
            res = x.g()
        else:
            res = None
        return res


    a = f(A())
    b = f(B())

    mopsa.assert_safe()
    mopsa.massert(isinstance(a, int))
    mopsa.massert(isinstance(b, int))

# A := Type[A]
# B := Type[B]
# f := Callable[[object], int]
# a := int
# b := int
