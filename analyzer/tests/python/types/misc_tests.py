import mopsa

def test_mro_pos():
    class O: pass
    class A(O): pass
    class B(O): pass
    class C(O): pass
    class D(O): pass
    class E(O): pass
    class K1(A, B, C): pass
    class K2(D, B, E): pass
    class K3(D, A): pass
    class Z(K1, K2, K3): pass

    a = Z()
    mopsa.massert(isinstance(a, Z))
    mopsa.massert(isinstance(a, K1))
    mopsa.massert(isinstance(a, K2))
    mopsa.massert(isinstance(a, K3))
    mopsa.massert(isinstance(a, D))
    mopsa.massert(isinstance(a, A))
    mopsa.massert(isinstance(a, B))
    mopsa.massert(isinstance(a, C))
    mopsa.massert(isinstance(a, E))
    mopsa.massert(isinstance(a, O))
    mopsa.massert(isinstance(a, object))

# def test_mro_neg():
#     class G: pass
#     class F: pass
#     class E(F): pass
#     class D(G): pass
#     class C(D, E): pass
#     class B(F, G): pass
#     class A(B, C): pass
#     mopsa.assert_exception(TypeError)

def test_attr_pos():
    class A: pass
    x = A()
    x.a = 3
    mopsa.massert(hasattr(x, 'a'))

# def test_attr_top():
#     class A: pass
#     x = A()
#     if mopsa.random_bool():
#         x.a = 3
#     else:
#         z = 3
