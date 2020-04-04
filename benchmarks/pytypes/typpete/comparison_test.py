import mopsa

def test_types():
    class A:
        def __init__(self, x):
            self.x = 1

        def __gt__(self, other):
            return self.x > other.x

        def __lt__(self, other):
            return self.x < other.x

        def __ge__(self, other):
            return self.x >= other.x

        def __le__(self, other):
            return self.x <= other.x


    a1 = A(1)
    a2 = A(2)

    b3 = a1 > a2
    b4 = a1 >= a2
    b5 = a1 < a2
    b6 = a1 <= a2

    b7 = "" > ""
    b8 = 1 > 1
    b9 = [1,2,3] > [4,6]
    b10 = {1} >= {3}

    mopsa.assert_safe()
    mopsa.massert(isinstance(a1, A))
    mopsa.massert(isinstance(a2, A))
    mopsa.massert(isinstance(a1.x, int))
    mopsa.massert(isinstance(a2.x, int))
    mopsa.massert(isinstance(b3, int))
    mopsa.massert(isinstance(b4, int))
    mopsa.massert(isinstance(b5, int))
    mopsa.massert(isinstance(b6, int))
    mopsa.massert(isinstance(b7, int))
    mopsa.massert(isinstance(b8, int))
    mopsa.massert(isinstance(b9, int))
    mopsa.massert(isinstance(b10, int))
