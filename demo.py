from c import typ, Cbox, broken


class A:
    def __init__(self, x):
        self.x = x

r0 = typ([])
# r0 = typ(0) # issue with integers, weak variables, it's a mess
# r1 = typ(A(1)) # FIXME
# r12 = typ(A)
# r2 = typ(Cbox)

a = A(1)

c = Cbox(A, 1)

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
