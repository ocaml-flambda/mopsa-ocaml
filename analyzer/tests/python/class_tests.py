import mopsa

# def test_init():
#     class C:
#         def __init__(self):
#             self.x = 10
#     c = C()
#     mopsa.assert_equal(c.x, 10)


# def test_static_attribute():
#     class C:
#         x = 100
#         def f(self):
#             return self.x

#     c1 = C()
#     mopsa.assert_equal(c1.x, 100)

# def test_static_attribute_change():
#     class C:
#         x = 100

#     c1 = C()
#     c2 = C()
#     c2.x = 200
#     mopsa.assert_equal(c1.x, 100)
#     C.x = 300
#     mopsa.assert_equal(c1.x, 300)
#     mopsa.assert_equal(c2.x, 200)

# def test_method_call():
#     class A:
#         x = 10
#         def f(self):
#             return self.x + 1

#     a = A()
#     mopsa.assert_equal(a.f(), 11)

def test_instance_in_condition():
    class C: pass

    c1 = C()
    if c1:
        x = 1
    else:
        x = 2
    if not c1:
        y = 1
    else:
        y = 2
    mopsa.assert_equal(x, 1)
    mopsa.assert_equal(y, 2)
