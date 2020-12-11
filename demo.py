from c import typ, Cbox, broken

r = typ()

class A:
    def __init__(self, x):
        self.x = x

a = A(1)

c = Cbox(a)

a.a = 42

ccontent = c.getcontents().a

b = broken()
# c2 = Cbox.__new__(Cbox)
# c2.getcontents()
