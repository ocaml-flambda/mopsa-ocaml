from c import typ, Cbox, broken

r = typ()

class A:
    def __init__(self, x):
        self.x = x

a = A(1)

c = Cbox(a)

a.a = 42

ccontent = c.contents #getcontents().a

cz = c.counter
cr = c.incr()
co = c.counter
b = broken()
