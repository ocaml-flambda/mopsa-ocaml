# type_params {'f': ['H']}

def f(x, y):
    return x(y)


def a(x: int):
    return x


def b(x: str):
    return object()


f(a, 1)
f(b, "str")


# a := Callable[[int], int]
# b := Callable[[str], object]