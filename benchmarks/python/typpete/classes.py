import mopsa

def test_types():
    class A:
        def __init__(self, x=1):
            pass

        def f(self):
            return 1

    class B:
        def f(self):
            return "string"

    def fab(a):
        return a().f()

    x = fab(B)

    class C:
        x = 1

    c = C()

    class D:
        def f(self):
            return 2

    class E(D):
        pass

    e = E()
    f = e.f()
    a = A()
    aa = A(1)

    mopsa.massert(isinstance(A, type))
    mopsa.massert(isinstance(B, type))
    mopsa.massert(isinstance(C, type))
    mopsa.massert(isinstance(D, type))
    mopsa.massert(isinstance(E, type))
    mopsa.massert(isinstance(x, str))
    mopsa.massert(isinstance(c, C))
    mopsa.massert(isinstance(e, E))
    mopsa.massert(isinstance(f, int))
    mopsa.massert(isinstance(aa, A))

# A := Type[A]
# B := Type[B]
# C := Type[C]
# D := Type[D]
# E := Type[E]
# a := A
# aa := A
# x := str
# c := C
# fab := Callable[[Type[B]], str]
