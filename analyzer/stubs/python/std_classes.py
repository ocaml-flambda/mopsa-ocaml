import mopsa

class object:
    def __new__(cls, args): pass
    def __init__(self, args): pass

    @mopsa.stub
    def __eq__(self, other): return NotImplemented

    @mopsa.stub
    def __ne__(self, other): return NotImplemented

    @mopsa.stub
    def __ge__(self, other): return NotImplemented

    @mopsa.stub
    def __gt__(self, other): return NotImplemented

    @mopsa.stub
    def __lt__(self, other): return NotImplemented

    @mopsa.stub
    def __le__(self, other): return NotImplemented


class int(object):
    def __new__(cls, args): pass

    def __eq__(self, other): pass
    def __ne__(self, other): pass
    def __ge__(self, other): pass
    def __gt__(self, other): pass
    def __lt__(self, other): pass
    def __le__(self, other): pass

    def __bool__(self): pass

    def __add__(self, other): pass
    def __sub__(self, other): pass
    def __mul__(self, other): pass
    def __matmul__(self, other): pass
    def __truediv__(self, other): pass
    def __floordiv__(self, other): pass
    def __mod__(self, other): pass
    def __pow__(self, other): pass
    def __divmod__(self, other): pass
    def __lshift__(self, other): pass
    def __rshift__(self, other): pass
    def __and__(self, other): pass
    def __xor__(self, other): pass
    def __or__(self, other): pass

    def __radd__(self, other): pass
    def __rsub__(self, other): pass
    def __rmul__(self, other): pass
    def __rmatmul__(self, other): pass
    def __rtruediv__(self, other): pass
    def __rfloordiv__(self, other): pass
    def __rmod__(self, other): pass
    def __rdivmod__(self, other): pass
    def __rpow__(self, other): pass
    def __rlshift__(self, other): pass
    def __rrshift__(self, other): pass
    def __rand__(self, other): pass
    def __rxor__(self, other): pass
    def __ror__(self, other): pass

    def __neg__(self): pass
    def __pos__(self): pass
    def __abs__(self): pass
    def __invert__(self): pass

class float(object):
    def __new__(cls, args): pass

    def __eq__(self, other): pass
    def __ne__(self, other): pass
    def __ge__(self, other): pass
    def __gt__(self, other): pass
    def __lt__(self, other): pass
    def __le__(self, other): pass

    def __bool__(self): pass

    def __add__(self, other): pass
    def __sub__(self, other): pass
    def __mul__(self, other): pass
    def __truediv__(self, other): pass
    def __floordiv__(self, other): pass
    def __pow__(self, other): pass
    def __mod__(self, other): pass
    def __divmod__(self, other): pass

    def __radd__(self, other): pass
    def __rsub__(self, other): pass
    def __rmul__(self, other): pass
    def __rtruediv__(self, other): pass
    def __rfloordiv__(self, other): pass
    def __rmod__(self, other): pass
    def __rdivmod__(self, other): pass
    def __rpow__(self, other): pass

    def __neg__(self): pass
    def __pos__(self): pass
    def __abs__(self): pass

class bool(int):
    def __new__(cls, args): pass

    def __eq__(self, other): pass
    def __ne__(self, other): pass
    def __ge__(self, other): pass
    def __gt__(self, other): pass
    def __lt__(self, other): pass
    def __le__(self, other): pass

    def __bool__(self): pass

    def __add__(self, other): pass
    def __sub__(self, other): pass
    def __mul__(self, other): pass
    def __matmul__(self, other): pass
    def __truediv__(self, other): pass
    def __floordiv__(self, other): pass
    def __pow__(self, other): pass
    def __mod__(self, other): pass
    def __divmod__(self, other): pass
    def __lshift__(self, other): pass
    def __rshift__(self, other): pass
    def __and__(self, other): pass
    def __xor__(self, other): pass
    def __or__(self, other): pass

    def __radd__(self, other): pass
    def __rsub__(self, other): pass
    def __rmul__(self, other): pass
    def __rmatmul__(self, other): pass
    def __rtruediv__(self, other): pass
    def __rfloordiv__(self, other): pass
    def __rmod__(self, other): pass
    def __rdivmod__(self, other): pass
    def __rpow__(self, other): pass
    def __rlshift__(self, other): pass
    def __rrshift__(self, other): pass
    def __rand__(self, other): pass
    def __rxor__(self, other): pass
    def __ror__(self, other): pass

    def __neg__(self): pass
    def __pos__(self): pass
    def __abs__(self): pass
    def __invert__(self): pass

class str(object):
    def __new__(cls, arg): pass

    def __eq__(self, other): pass
    def __ne__(self, other): pass
    def __ge__(self, other): pass
    def __gt__(self, other): pass
    def __lt__(self, other): pass
    def __le__(self, other): pass

    def __bool__(self): pass
    def __len__(self): pass

    def splitlines(self): pass

class list(object):
    def __init__(self, itr): pass
    def __len__(self): pass
    def __getitem__(self, k): pass
    def __setitem__(self, k, v): pass
    def __iter__(self): pass
    def __add__(self, o): pass
    def __iadd__(self, o): pass
    def __mul__(self, o): pass
    def __imul__(self, o): pass
    def __eq__(self, o): pass
    def __ne__(self, o): pass
    def __contains__(self, v): pass
    def append(self, x): pass
    def insert(self, x): pass
    def pop(self): pass

class listiter(object):
    def __next__(self): pass

class dict(object):
    def __getitem__(self, k): pass
    def __setitem__(self, k, v): pass
    def values(self): pass

class dict_values(object):
    def __iter__(self): pass

class dict_valueiterator(object):
    def __next__(self): pass

class range(object):
    def __new__(cls, args): pass
    def __len__(self): pass
    def __iter__(self): pass
    def __contains__(self, v): pass
    def __getitem__(self, k): pass

class rangeiter(object):
    def __next__(self): pass

class set(object):
    def __init__(self, start, stop): pass
    def __len__(self): pass
    def __iter__(self): pass
    def __contains__(self, v): pass
    def add(self, c): pass
    def clear(self): pass

class tuple(object):
    def __init__(self): pass
    def __iter__(self): pass
    def __getitem__(self, k): pass

class tupleiter(object):
    def __next__(self): pass

class slice(object):
    def __new__(self, args): pass

class NotImplementedType(object): pass
class NoneType(object): pass

@mopsa.unsupported
class bytearray(object): pass

@mopsa.unsupported
class bytes(object): pass

@mopsa.unsupported
class classmethod(object):pass

@mopsa.unsupported
class frozenset(object): pass

@mopsa.unsupported
class map(object): pass

@mopsa.unsupported
class memoryview(object): pass

@mopsa.unsupported
class property(object): pass

@mopsa.unsupported
class reversed(object): pass

@mopsa.unsupported
class staticmethod(object): pass

@mopsa.unsupported
class super(object): pass

@mopsa.unsupported
class zip(object): pass
