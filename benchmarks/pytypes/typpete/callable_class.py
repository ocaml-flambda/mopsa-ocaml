import mopsa

def test_main():
    class A:
        def __init__(self):
            self.x = 1
            self.y = 2.0

        def __call__(self, z):
            return z + [self.x + self.y]

    x = A()
    y = x([])

    mopsa.ignore_exception(OverflowError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(x, A))
    mopsa.massert(isinstance(y, list))
    mopsa.massert(isinstance(y[0], float))
    mopsa.ignore_exception(IndexError)

# A := Type[A]
# __call__ := Callable[[A, List[float]], List[float]]
# x := A
# y := List[float]
