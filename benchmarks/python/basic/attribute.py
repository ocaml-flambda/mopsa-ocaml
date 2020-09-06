class B:
    y = 2
    def f(self, x):
        return x + y

def factory():
    b = B()
    return b

b1 = factory()
b1.z = 1

b2 = factory()
b2.z = 2

g = b2.f

b3 = factory()
b3.z = 3

# g = b3.f

# b4 = factory()
# b4.z = 3

# g = b4.f
# n2 = B.y
# n3 = b4.y
# n4 = g(10)
