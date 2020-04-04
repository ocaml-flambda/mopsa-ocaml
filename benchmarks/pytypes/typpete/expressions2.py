import mopsa


def test_types():
    condition = True
    q = [3] if condition else [2]
    r = [2.0] and [1, 2]
    rr = r[0] * 2
    mopsa.ignore_exception(OverflowError)
    s = q + r
    t = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    u = 2
    v = 8
    w = t[u:v:u]
    x = w[u]

    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(condition, bool))
    mopsa.assert_list_of(q, int)
    # todo: fixme and conflict with annotation below
    # mopsa.assert_list_of(r, int)
    # mopsa.massert(isinstance(rr, int))
    # mopsa.assert_list_of(s, float)
    mopsa.assert_list_of(t, int)
    mopsa.massert(isinstance(u, int))
    mopsa.massert(isinstance(v, int))
    mopsa.massert(isinstance(x, int))
    mopsa.assert_list_of(w, int)

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
