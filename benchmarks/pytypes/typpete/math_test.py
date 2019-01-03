import math
import mopsa

def test_main():
    x = math.pi

    a = math.acos(x)
    b = math.acosh(x)
    c = math.asin(x)
    d = math.asinh(x)
    e = math.atan(x)
    f = math.atan2(x, a)
    g = math.atanh(x)

    h = math.ceil(x)
    i = math.copysign(x, f)
    j = math.cos(x)
    k = math.cosh(x)
    l = math.degrees(x)
    m = math.erf(x)
    n = math.erfc(x)
    o = math.exp(x)
    p = math.expm1(x)
    q = math.fabs(x)
    r = math.factorial(math.floor(x))
    s = math.fmod(x, n)
    t, u = math.frexp(x)
    v = math.fsum([a, b, c, d])
    w = math.gamma(x)
    gcd = math.gcd(r, u)
    y = math.hypot(x, a)
    z = math.isclose(x, y)
    aa = math.isclose(x, y)
    ab = math.isfinite(x)
    ac = math.isinf(x)
    ad = math.isnan(x)
    ae = math.ldexp(x, gcd)
    af = math.lgamma(x)
    ag = math.log(x, y)
    ah = math.log10(x)
    ai = math.log1p(x)
    aj = math.log2(x)
    ak = math.modf(x)
    am = math.radians(x)
    an = math.sin(x)
    ao = math.sinh(x)
    ap = math.sqrt(x)
    aq = math.tan(x)
    ar = math.tanh(x)

    mopsa.assert_safe()
    mopsa.massert(isinstance(a, float))
    mopsa.massert(isinstance(aa, bool))
    mopsa.massert(isinstance(ab, bool))
    mopsa.massert(isinstance(ac, bool))
    mopsa.massert(isinstance(ad, bool))
    mopsa.massert(isinstance(ae, float))
    mopsa.massert(isinstance(af, float))
    mopsa.massert(isinstance(ag, float))
    mopsa.massert(isinstance(ah, float))
    mopsa.massert(isinstance(ai, float))
    mopsa.massert(isinstance(aj, float))
    mopsa.assert_tuple_of(ak, (float, float))
    mopsa.massert(isinstance(am, float))
    mopsa.massert(isinstance(an, float))
    mopsa.massert(isinstance(ao, float))
    mopsa.massert(isinstance(ap, float))
    mopsa.massert(isinstance(aq, float))
    mopsa.massert(isinstance(ar, float))
    mopsa.massert(isinstance(b, float))
    mopsa.massert(isinstance(c, float))
    mopsa.massert(isinstance(d, float))
    mopsa.massert(isinstance(e, float))
    mopsa.massert(isinstance(f, float))
    mopsa.massert(isinstance(g, float))
    mopsa.massert(isinstance(gcd, int))
    mopsa.massert(isinstance(h, int))
    mopsa.massert(isinstance(i, float))
    mopsa.massert(isinstance(j, float))
    mopsa.massert(isinstance(k, float))
    mopsa.massert(isinstance(l, float))
    mopsa.massert(isinstance(m, float))
    mopsa.massert(isinstance(n, float))
    mopsa.massert(isinstance(o, float))
    mopsa.massert(isinstance(p, float))
    mopsa.massert(isinstance(q, float))
    mopsa.massert(isinstance(r, int))
    mopsa.massert(isinstance(s, float))
    mopsa.massert(isinstance(t, float))
    mopsa.massert(isinstance(u, int))
    mopsa.massert(isinstance(v, float))
    mopsa.massert(isinstance(w, float))
    mopsa.massert(isinstance(x, float))
    mopsa.massert(isinstance(y, float))
    mopsa.massert(isinstance(z, bool))

# a := float
# aa := bool
# ab := bool
# ac := bool
# ad := bool
# ae := float
# af := float
# ag := float
# ah := float
# ai := float
# aj := float
# ak := Tuple[float, float]
# am := float
# an := float
# ao := float
# ap := float
# aq := float
# ar := float
# b := float
# c := float
# d := float
# e := float
# f := float
# g := float
# gcd := int
# h := int
# i := float
# j := float
# k := float
# l := float
# m := float
# n := float
# o := float
# p := float
# q := float
# r := int
# s := float
# t := float
# u := int
# v := float
# w := float
# x := float
# y := float
# z := bool
