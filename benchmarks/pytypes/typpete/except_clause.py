class MyException(Exception):
    def __init__(self):
        self.val = 15


try:
    a = 23
    raise MyException
except MyException as e:
    b = e.val
    a = b + 2


# a := int
# b := int
