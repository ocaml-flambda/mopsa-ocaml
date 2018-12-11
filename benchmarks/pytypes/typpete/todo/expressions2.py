import mopsa


def test_main():
    condition = True
    q = [3] if condition else [2]
    r = [2.0] and [1, 2]
    rr = r[0] * 2
    s = q + r
    t = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    u = 2
    v = 8
    w = t[u:v:u]
    x = w[u]

    mopsa.assert_safe()
    mopsa.massert(isinstance(condition, bool))
    mopsa.massert(isinstance(q, list))
    mopsa.massert(isinstance(q[0], int))
    mopsa.massert(isinstance(r, list))
    mopsa.massert(isinstance(r[0], int))
    mopsa.massert(isinstance(rr, int))
    mopsa.massert(isinstance(s, list))
    mopsa.massert(isinstance(s[0], float))
    mopsa.massert(isinstance(t, list))
    mopsa.massert(isinstance(t[0], int))
    mopsa.massert(isinstance(u, int))
    mopsa.massert(isinstance(v, int))
    mopsa.massert(isinstance(x, int))
    mopsa.massert(isinstance(w, list))
    mopsa.massert(isinstance(w[0], int))

# q := List[float]
# ?!? q is supposed to be List[int]
# same for r... and rr
# r := List[float]
# rr := float
# s := List[float]
# t := List[int]
# u := int
# v := int
# w := List[int]
# x := int
