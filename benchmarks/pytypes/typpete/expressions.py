import mopsa

def test_main():
    a = 1
    b = [a, 2]
    c = {a: b[0]}
    d = (a, b, c)
    e = {a}
    f = (a & 2 * 3) - 33 / 2
    g = b + [1]
    h = ~a
    i = a if a else h
    k = b[1:2]
    l = [(x, x * 2) for x in b]
    m = {x : y for x in b for y in e}
    n, o, p = 1, "st", True
    nn = n
    oo = o
    pp = p

    mopsa.assert_safe()
    mopsa.massert(isinstance(a, int))
    mopsa.assert_list_of(b, int)
    mopsa.assert_dict_of(c, int, int)
    mopsa.massert(isinstance(d, tuple))
    mopsa.assert_set_of(e, int)
    mopsa.massert(isinstance(f, float))
    mopsa.assert_list_of(g, int)
    mopsa.massert(isinstance(h, int))
    mopsa.massert(isinstance(i, int))
    mopsa.assert_list_of(k, int)
    mopsa.massert(isinstance(l, list))
    mopsa.massert(isinstance(l[0], tuple))
    mopsa.massert(isinstance(l[0][0], int))
    mopsa.assert_dict_of(m, int, int)
    mopsa.massert(isinstance(n, int))
    mopsa.massert(isinstance(o, str))
    mopsa.massert(isinstance(p, bool))



# a := int
# b := List[int]
# c := Dict[int, int]
# d := Tuple[int, List[int], Dict[int, int]]
# e := Set[int]
# f := float
# g := List[int]
# h := int
# i := int
# k := List[int]
# l := List[Tuple[int, int]]
# m := Dict[int, int]
# n := int
# o := str
# p := bool
