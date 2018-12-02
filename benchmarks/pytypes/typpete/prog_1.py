import mopsa

def test_main():
    n, m = 1, 2
    a = [''] * n
    for i in range(n):
        a[n - i - 1] = input()

    res, ll, rr, k, l, r = 0, 0, 0, 0, 0, 0

    for s in a:
        ss = s
        i, l, r = 0, 0, 0
        for z in s:
            zz = z
            if z == '1':
                if l == 0:
                    l = m - i + 1
                r = i
            i += 1
        if k == 0:
            p, q, rr = ll + 2 * r, ll + m + 1, m + 1
        else:
            p, q = 1, 2
        if l or r:
            res = 1
        k += 1
        ll, rr = p, q

    mopsa.assert_safe()
    mopsa.massert(isinstance(a, list))
    mopsa.massert(isinstance(a[0], str))
    mopsa.massert(isinstance(i, int))
    mopsa.massert(isinstance(k, int))
    mopsa.massert(isinstance(l, int))
    mopsa.massert(isinstance(ll, int))
    mopsa.massert(isinstance(m, int))
    mopsa.massert(isinstance(n, int))
    mopsa.massert(isinstance(p, int))
    mopsa.massert(isinstance(q, int))
    mopsa.massert(isinstance(r, int))
    mopsa.massert(isinstance(res, int))
    mopsa.massert(isinstance(rr, int))
    mopsa.massert(isinstance(s, str))
    mopsa.massert(isinstance(z, str))

# a := List[str]
# i := int
# k := int
# l := int
# ll := int
# m := int
# n := int
# p := int
# q := int
# r := int
# res := int
# rr := int
# s := str
# z := str
