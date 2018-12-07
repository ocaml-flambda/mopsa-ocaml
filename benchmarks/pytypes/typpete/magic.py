import mopsa

def test_main():
    T = 3

    for x in [1, 2, 3, T + 1]:

        l1 = 2
        for i in [1, l1]:
            pass
        L1 = [9, 10, 11, 12]
        for i in [l1 + 1, 4]:
            pass

        l2 = 3
        for i in [1, 2, l2]:
            pass
        L2 = [9, 10, 7, 12]
        for i in [l2 + 1]:
            pass

        z = 0
        n = 0
        for i in [0, 1, 2, 3]:
            for j in [0, 1, 2, 3]:
                if L1[i] == L2[j]:
                    z = z + 1
                    n = L1[i]

        if z == 1:
            res = ""
        else:
            if z == 0:
                res = ""
            else:
                res = ""

    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(T, int))
    mopsa.massert(isinstance(x, int))
    mopsa.massert(isinstance(l1, int))
    mopsa.massert(isinstance(l2, int))
    mopsa.massert(isinstance(i, int))
    mopsa.massert(isinstance(j, int))
    mopsa.massert(isinstance(z, int))
    mopsa.massert(isinstance(n, int))

    mopsa.massert(isinstance(L1, list))
    mopsa.massert(isinstance(L2, list))
    mopsa.massert(isinstance(L1[0], int))
    mopsa.massert(isinstance(L2[0], int))
    mopsa.ignore_exception(IndexError)
    mopsa.massert(isinstance(res, str))

# T := int
# x := int
# l1 := int
# L1 := List[int]
# l2 := int
# L2 := List[int]
# i := int
# j := int
# z := int
# n := int
# res := str
