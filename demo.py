from c import typ, Cbox, broken

r = typ()

class A:
    def __init__(self, x):
        self.x = x

a = A(1)

c = Cbox(a)

# a.a = 42

# ccontent = c.contents #getcontents().a

# cz = c.counter
# cr = c.incr()
# # c.contents = 42
# c.counter = 4
# cf = c.counter
# c.counter = 10**10
# # c.counter = 10**1000 # should raise an OverflowError
# d = c.counter
# b = broken()
