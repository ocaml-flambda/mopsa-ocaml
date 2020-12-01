from c import Cbox

class A: pass

a = A()

c = Cbox(a)

a.a = 42

assert(c.contents.a == 42)
