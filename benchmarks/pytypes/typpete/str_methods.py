import mopsa

def test_main():
    a = "abcd"
    b = a.capitalize()
    c = b.center(4, "a")
    d = (c.isalnum() or a.isalpha() or a.isdecimal()
         or a.isdecimal() or a.isdigit() or a.islower()
         or a.isnumeric() or a.isspace() or a.istitle()
         or a.isupper())
    e = a.join([b])
    f = e.lower()
    g = a.replace(f, "string")
    h = g.rstrip()
    i = h.split(" ")
    j = i[0].strip()
    k = j.swapcase()
    l = k.title()
    m = l.upper()
    n = m[d]
    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(a, str))
    mopsa.massert(isinstance(b, str))
    mopsa.massert(isinstance(c, str))
    mopsa.massert(isinstance(e, str))
    mopsa.massert(isinstance(f, str))
    mopsa.massert(isinstance(g, str))
    mopsa.massert(isinstance(h, str))
    mopsa.massert(isinstance(i, list))
    mopsa.massert(isinstance(i[0], str))
    mopsa.ignore_exception(IndexError)
    mopsa.massert(isinstance(j, str))
    mopsa.massert(isinstance(k, str))
    mopsa.massert(isinstance(l, str))
    mopsa.massert(isinstance(m, str))
    mopsa.massert(isinstance(n, str))
# a := str
# b := str
# c := str
# e := str
# f := str
# g := str
# h := str
# i := List[str]
# j := str
# k := str
# l := str
# m := str
