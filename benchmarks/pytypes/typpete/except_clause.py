import mopsa

def test_main():
    class MyException(Exception):
        def __init__(self):
            self.val = 15


    try:
        a = 23
        raise MyException
    except MyException as e:
        b = e.val
        a = b + 2

    mopsa.massert(isinstance(a, int))
    mopsa.massert(isinstance(b, int))
# a := int
# b := int
