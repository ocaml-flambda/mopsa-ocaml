import mopsa


def test_main():
    class A:
        def f(self):
            return 1


    class B:
        def g(self):
            return "str"


    class C(A, B):
        pass


    def a(x):
        return x.f()


    def b(x):
        return x.g()


    def c(x):
        return x.g() + str(x.f())


    z1 = a(A())
    z2 = a(C())

    z3 = b(B())
    z4 = b(C())

    z5 = c(C())

    mopsa.assert_safe()
    mopsa.massert(isinstance(z1, int))
    mopsa.massert(isinstance(z2, int))

    mopsa.massert(isinstance(z3, str))
    mopsa.massert(isinstance(z4, str))
    mopsa.massert(isinstance(z5, str))


# A := Type[A]
# B := Type[B]
# C := Type[C]
# a := Callable[[A], int]
# b := Callable[[B], str]
# c := Callable[[C], str]

# z1 := int
# z2 := int

# z3 := str
# z4 := str

# z5 := str
