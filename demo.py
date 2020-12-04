from c import typ, Cbox, broken

# r = broken()

class A:
    def __init__(self, x):
        self.x = x

a = A(1)

c = Cbox(a)

# a.a = 42

# assert(c.contents.a == 42)
