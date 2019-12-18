def fspath(p):
    # a simplified version of os.fspath
    # same example with **custom** classes would not be analyzable
    if isinstance(p, str) or isinstance(p, bytes):
        return p
    elif hasattr(p, "__fspath__"):
        res = p.__fspath__()
        if isinstance(res, str) or isinstance(res, bytes):
            return res
        else:
            raise TypeError("__fspath__ should return str or bytes")
    else:
        raise TypeError("input should have type str, bytes or attribute __fspath__, found type %s instead")

class FSPath1: pass

class FSPath2:
    def __fspath__(self):
        return 42

class FSPath3:
    def __fspath__(self):
        return "bli"

def main():
    f1 = fspath("bla")
    f2 = fspath(b'bla')
    f3 = fspath(FSPath3())
    f4 = fspath(FSPath2())

if __name__ == "__main__":
    main()
