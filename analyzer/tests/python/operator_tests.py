import mopsa

# def test_augmented_assign():
#     class C:
#         def __init__(self, x):
#             self.x = x

#         def __iadd__(self, other):
#             self.x = self.x + other * 10
#             return self

#     c = C(5)
#     c += 2
#     mopsa.assert_equal(c.x, 25)

def test_add():
    class C:
        def __init__(self, x, y):
            self.x = x
            self.y = y

        def __add__(self, other):
            return C(self.x + other.x, self.y + other.y)

    c1 = C(1, 2)
    c2 = C(3, 4)
    c3 = c1 + c2
    mopsa.assert_equal(c3.x, 4)
