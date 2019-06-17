# type_params {'f': ['H']}
import mopsa

def test_main():
    def f(x, y):
        return x(y)


    def a(x: int):
        return x


    def b(x: str):
        return object()


    r = f(a, 1)
    s = f(b, "str")
    mopsa.massert(isinstance(r, int))
    mopsa.massert(isinstance(s, object))

# a := Callable[[int], int]
# b := Callable[[str], object]
