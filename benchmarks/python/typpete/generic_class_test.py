# class_type_params {'Cell': ['V']}

import mopsa


def test_types():
    class Cell:
        def __init__(self, val):
            self.value = val

        def cget(self):
            return self.value

        def cset(self, val):
            self.value = val


    class A:
        def foo(self):
            return 12

    c1 = Cell(A())

    my_a = c1.cget()
    my_b = c1.value
    my_a = my_b
    c1.cset(A())

    my_a.foo()

    c2 = Cell(2)

    a55 = c2.value + 23
    mt = {1:"hi"}
    mt2 = mt.copy()

    a1 = c2.cget() + 1

    mopsa.assert_safe()
    mopsa.massert(isinstance(my_a, A))
    mopsa.massert(isinstance(my_b, A))
    mopsa.massert(isinstance(c1, Cell))
    mopsa.massert(isinstance(c2, Cell))

    mopsa.massert(isinstance(a55, int))
    mopsa.assert_dict_of(mt, int, str)
    mopsa.assert_dict_of(mt2, int, str)
    mopsa.massert(isinstance(a1, int))



# cget := Callable[[Cell[V]], V]
# cset := Callable[[Cell[V], V], None]
# my_a := A
