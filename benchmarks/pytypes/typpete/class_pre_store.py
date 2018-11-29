import mopsa

def test_main():
    class A:
        def f(self):
            return B()

        def g(self):
            return self.f().g()

    class B(A):
        def g(self):
            return "string"

    x = A()
    y = x.g()

    mopsa.massert(isinstance(A, type))
    mopsa.massert(isinstance(B, type))
    mopsa.massert(isinstance(x, A))
    mopsa.massert(isinstance(y, str))

# A := Type[A]
# B := Type[B]
# x := A
# y := str
