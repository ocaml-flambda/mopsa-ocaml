import mopsa

def test_types():
    class A:
        @staticmethod
        def f():
            return 1

        def g(self):
            return A.f()

    x = A.f()
    y = A().g()

    mopsa.assert_safe()
    mopsa.massert(isinstance(x, int))
    mopsa.massert(isinstance(y, int))


# A := Type[A]
# f := Callable[[], int]
# g := Callable[[A], int]

# x := int
# y := int
