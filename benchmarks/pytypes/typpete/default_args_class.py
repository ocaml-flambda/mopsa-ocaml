import mopsa

def test_types():
    class A:
        def __init__(self, x=1):
            pass


    def f(A):
        x = A()
        y = A(1)

    z = f(A)

    x = A()
    y = A(1)

    mopsa.massert(isinstance(A, type))
    mopsa.massert(isinstance(x, A))
    mopsa.massert(isinstance(y, A))
    mopsa.massert(isinstance(z, type(None)))

# A := Type[A]
# f := Callable[[Type[A]], None]
# x := A
# y := A
