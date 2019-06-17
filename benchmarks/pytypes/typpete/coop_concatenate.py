# class_type_params {'Concatenator': [0]}
# type_params {'concatenate': [1,2]}

import mopsa

def test_main():
    class Concatenator:
        def __init__(self, a):
            self.a = a


        def concatenate(self, separator, from_, to):
            self.a = separator
            to.append(separator)
            for f in from_:
                to.append(f)


    class Z:
        def foo(self):
            pass

    class Y(Z):
        def bar(self):
            pass

    class X(Y):
        def baz(self):
            pass

    class W(X):
        def omg(self):
            pass


    concatenator = Concatenator(Y())
    concatenator.a.bar()

    mopsa.assert_safe()
    mopsa.massert(isinstance(concatenator, Concatenator))


    wlst = [W()]
    wlst[0].omg()
    xlst = [X()]
    xlst[0].baz()
    concatenator.concatenate(X(), wlst, xlst)

    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(wlst, list))
    mopsa.massert(isinstance(xlst, list))
    mopsa.massert(isinstance(wlst[0], W))
    mopsa.massert(isinstance(xlst[0], X))

    class ZP:
        def foo(self):
            pass

    class YP(ZP):
        def bar(self):
            pass

    class XP(YP):
        def baz(self):
            pass

    class WP(XP):
        def omg(self):
            pass

    concatenatorP = Concatenator(YP())
    concatenatorP.a.bar()

    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(concatenatorP, Concatenator))

    wlstP = [WP()]
    wlstP[0].omg()
    xlstP = [XP()]
    xlstP[0].baz()
    concatenatorP.concatenate(XP(), wlstP, xlstP)

    mopsa.ignore_exception(IndexError)
    mopsa.assert_safe()
    mopsa.massert(isinstance(wlstP, list))
    mopsa.massert(isinstance(xlstP, list))
    mopsa.massert(isinstance(wlstP[0], WP))
    mopsa.massert(isinstance(xlstP[0], XP))
    mopsa.ignore_exception(IndexError)
