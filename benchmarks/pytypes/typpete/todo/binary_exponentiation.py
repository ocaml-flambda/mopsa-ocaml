# """Binary exponentiation"""
# from typing import TypeVar

#AnyNum = TypeVar("Num", bool, int, float, complex)


#def exp(a: AnyNum, b: int) -> AnyNum:
def exp(a, b: int):
    res = 1
    while b > 0:
        print(res)
        if b & 1 != 0:
            res = res * a
        a = a * a
        b >>= 1
    return res


def f(x):
    a = exp(x, 2)
    b = exp(2.0, x)

    return a * b

#import mopsa

#def test_main():
x = exp(1, 2)
y = exp(2.0, 3)
    # mopsa.assert_safe()
    # mopsa.massert(isinstance(x, int))
    # # without annotations, typpete is unable to find the good types
    # too however, the real type of y is float, not int \/ float, but
    # it's difficult to infer using only types that we don't enter the
    # while loop above
    # mopsa.massert(isinstance(y, float))
# z1 = isinstance(y, (int, float))
# if isinstance(y, int):
#     z = "a"
# else:
#     if isinstance(y, float):
#         z = 3
#     else:
#         z = []
z1 = True or False
z2 = isinstance(y, int) or isinstance(y, float)
# assert(isinstance(y, int) or isinstance(y, float))

# f := Callable[[int], float]
# x := int
# y := float
