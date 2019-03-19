import mopsa

def test_main():
    a = 1 + 2 / 3
    a += 0
    b = -a
    c = [1.2, 2.0, b]
    d = [[1.1, 2.5], c]
    e = {(i, i * 2) for j in d for i in j}
    f = 2 & 3
    mopsa.assert_safe()
    g = d[f]
    mopsa.ignore_exception(IndexError)
    h = (g, 2.0, a)
    i, j, (k, (l, m)) = {1: 2.0}, True, ((1, 2), (f, e))
    ii = i
    jj = j
    kk = k
    ll = l
    mm = m
    n = a if True else "string"
    o = (1 is 2) + 1
    mopsa.assert_safe()
    p = i[o]
    mopsa.ignore_exception(KeyError)

    mopsa.assert_safe()
    mopsa.massert(isinstance(a, float))
    mopsa.massert(isinstance(b, float))
    mopsa.assert_list_of(c, float)
    mopsa.massert(isinstance(d, list))
    mopsa.massert(isinstance(d[0], list))
    mopsa.massert(isinstance(d[0][0], float))
    mopsa.ignore_exception(IndexError)
    mopsa.massert(isinstance(e, set))
    mopsa.massert(isinstance(f, int))
    mopsa.assert_list_of(g, float)
    mopsa.massert(isinstance(h[0], list))
    mopsa.massert(isinstance(h[1], float))
    mopsa.massert(isinstance(h[2], float))
    # mopsa.assert_list_of(h[0], float)
    # mopsa.massert(h[1], float)
    # mopsa.massert(h[2], float)
    mopsa.assert_dict_of(i, int, float)
    mopsa.massert(isinstance(j, bool))
    mopsa.massert(isinstance(k[0], int))
    mopsa.massert(isinstance(k[1], int))
    # mopsa.massert(isinstance(k[0], int))
    # mopsa.massert(isinstance(k[1], int))
    mopsa.massert(isinstance(l, int))
    mopsa.massert(isinstance(m, set))
    mopsa.massert(isinstance(n, float))
    mopsa.massert(isinstance(o, int))
    mopsa.massert(isinstance(p, float))

# a := float
# b := float
# c := List[float]
# d := List[List[float]]
# e := Set[Tuple[float, float]]
# f := int
# g := List[float]
# h := Tuple[List[float], float, float]
# i := Dict[int, float]
# j := bool
# k := Tuple[int, int]
# l := int
# m := Set[Tuple[float, float]]
# n := object
# o := int
# p := float
