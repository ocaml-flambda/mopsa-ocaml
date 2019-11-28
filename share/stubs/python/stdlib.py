##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2019 The MOPSA Project.                                     #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################

import mopsa


###############
##  Classes  ##
###############

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

    def __format__(self): pass
    def __getattribute__(self, attr): pass
    def __hash__(self, attr): pass
    def __reduce__(self): pass
    def __setattr__(self, attr, v): pass
    def __sizeof__(self): pass
    def __str__(self): pass

class type(object):
    def __new__(cls, args): pass
    def __getattribute__(self, attr): pass
    def __init__(self): pass
    def __call__(self, args): pass
    def __repr__(self): pass

class function(object):
    def __new__(cls, args): pass
    def __call__(self, args): pass
    def __get__(self): pass
    def __repr__(self): pass

class method(object):
    def __new__(cls, args): pass
    def __call__(self, args): pass

class module(object):
    def __new__(cls, args): pass

class int(object):
    def __abs__(self): pass
    def __add__(self, other): pass
    def __and__(self, other): pass
    def __bool__(self): pass
    def __ceil__(self): pass
    def __divmod__(self, other): pass
    def __eq__(self, other): pass
    def __float__(self): pass
    def __floor__(self): pass
    def __floordiv__(self, other): pass
    def __format__(self): pass
    def __ge__(self, other): pass
    # def __getattribute__(self, attr): pass
    def __getnewargs__(self): pass
    def __gt__(self, other): pass
    def __hash__(self): pass
    def __index__(self): pass
    def __int__(self): pass
    def __invert__(self): pass
    def __le__(self, other): pass
    def __lshift__(self, other): pass
    def __lt__(self, other): pass
    def __mod__(self, other): pass
    def __mul__(self, other): pass
    def __ne__(self, other): pass
    def __neg__(self): pass
    def __new__(cls, args): pass
    def __or__(self, other): pass
    def __pow__(self, other): pass
    def __pos__(self): pass
    def __radd__(self, other): pass
    def __rand__(self, other): pass
    def __rdivmod__(self, other): pass
    def __repr__(self): pass
    def __rfloordiv__(self, other): pass
    def __rshift__(self, other): pass
    def __rmod__(self, other): pass
    def __rmul__(self, other): pass
    def __ror__(self, other): pass
    def __round__(self): pass
    def __rpow__(self, other): pass
    def __rrshift__(self, other): pass
    def __rshift__(self, other): pass
    def __rsub__(self, other): pass
    def __rtruediv__(self, other): pass
    def __rxor__(self, other): pass
    def __sizeof__(self): pass
    def __str__(self): pass
    def __sub__(self, other): pass
    def __truediv__(self, other): pass
    def __xor__(self, other): pass

class bool(int):
    def __and__(self, other): pass
    def __new__(self, args): pass
    def __or__(self, other): pass
    def __rand__(self, other): pass
    def __repr__(self): pass
    def __ror__(self, other): pass
    def __rxor__(self, other): pass
    def __str__(self): pass
    def __xor__(self, other): pass


class float(object):
    def __abs__(self): pass
    def __add__(self, other): pass
    def __bool__(self): pass
    def __divmod__(self, other): pass
    def __eq__(self, other): pass
    def __float__(self): pass
    def __floordiv__(self, other): pass
    def __format__(self): pass
    def __ge__(self, other): pass
    # def __getattribute__(self, attr): pass
    def __getnewargs__(self): pass
    def __gt__(self, other): pass
    def __hash__(self): pass
    def __int__(self): pass
    def __le__(self, other): pass
    def __lt__(self, other): pass
    def __mod__(self, other): pass
    def __mul__(self, other): pass
    def __ne__(self, other): pass
    def __neg__(self): pass
    def __new__(cls, args): pass
    def __pow__(self, other): pass
    def __pos__(self): pass
    def __radd__(self, other): pass
    def __rdivmod__(self, other): pass
    def __repr__(self): pass
    def __rfloordiv__(self, other): pass
    def __rmod__(self, other): pass
    def __rmul__(self, other): pass
    def __round__(self): pass
    def __rpow__(self, other): pass
    def __rsub__(self, other): pass
    def __rtruediv__(self, other): pass
    def __sizeof__(self): pass
    def __str__(self): pass
    def __sub__(self, other): pass
    def __truediv__(self, other): pass
    def __trunc__(self): pass

class complex(object):
    def __abs__(self): pass
    def __add__(self, other): pass
    def __bool__(self): pass
    def __divmod__(self, other): pass
    def __eq__(self, other): pass
    def __float__(self): pass
    def __floordiv__(self, other): pass
    def __format__(self): pass
    def __ge__(self, other): pass
    # def __getattribute__(self, attr): pass
    def __getnewargs__(self): pass
    def __gt__(self, other): pass
    def __hash__(self): pass
    def __int__(self): pass
    def __le__(self, other): pass
    def __lt__(self, other): pass
    def __mod__(self, other): pass
    def __mul__(self, other): pass
    def __ne__(self, other): pass
    def __neg__(self): pass
    def __new__(cls, args): pass
    def __pow__(self, other): pass
    def __pos__(self): pass
    def __radd__(self, other): pass
    def __rdivmod__(self, other): pass
    def __repr__(self): pass
    def __rfloordiv__(self, other): pass
    def __rmod__(self, other): pass
    def __rmul__(self, other): pass
    def __rpow__(self, other): pass
    def __rsub__(self, other): pass
    def __rtruediv__(self, other): pass
    def __sizeof__(self): pass
    def __str__(self): pass
    def __sub__(self, other): pass
    def __truediv__(self, other): pass
    def conjugate(self): pass

class str(object):
    def __add__(self, other): pass
    def __contains__(self, other): pass
    def __eq__(self, other): pass
    def __format__(self): pass
    def __ge__(self, other): pass
    # def __getattribute__(self, attr): pass
    def __getitem__(self, i): pass
    def __getnewargs__(self): pass
    def __gt__(self, other): pass
    def __hash__(self): pass
    def __iter__(self): pass
    def __le__(self, other): pass
    def __len__(self): pass
    def __lt__(self, other): pass
    def __mod__(self, other): pass
    def __mul__(self, other): pass
    def __ne__(self, other): pass
    def __new__(cls, args): pass
    def __repr__(self): pass
    def __rmod__(self, other): pass
    def __rmul__(self, other): pass
    def __sizeof__(self): pass
    def __str__(self): pass
    def capitalize(self): pass
    def casefold(self): pass
    def center(self): pass
    def count(self): pass
    def encode(self): pass
    def endswith(self): pass
    def expandtabs(self): pass
    def find(self): pass
    def format(self): pass
    def format(self): pass
    def index(self): pass
    def isalnum(self): pass
    def isalpha(self): pass
    def isdecimal(self): pass
    def isdigit(self): pass
    def isidentifier(self): pass
    def islower(self): pass
    def isnumeric(self): pass
    def isprintable(self): pass
    def isspace(self): pass
    def istitle(self): pass
    def isupper(self): pass
    def join(self): pass
    def ljust(self): pass
    def lower(self): pass
    def lstrip(self): pass
    def maketrans(self): pass
    def partition(self): pass
    def replace(self): pass
    def rfind(self): pass
    def rindex(self): pass
    def rjust(self): pass
    def rpartition(self): pass
    def rsplit(self): pass
    def rstrip(self): pass
    def split(self): pass
    def splitlines(self): pass
    def startswith(self): pass
    def strip(self): pass
    def swapcase(self): pass
    def title(self): pass
    def translate(self): pass
    def upper(self): pass
    def zfill(self): pass

class str_iterator:
    def __next__(self): pass

class generator(object):
    def __del__(self): pass
    def __eq__(self): pass
    def __format__(self): pass
    # def __getattribute__(self): pass
    def __iter__(self): pass
    def __repr__(self): pass
    def __next__(self): pass
    def close(self): pass
    def send(self): pass
    def throw(self): pass

class list(object):
    def __add__(self, o): pass
    def __contains__(self, v): pass
    def __delitem__(self, k): pass
    def __eq__(self, o): pass
    def __ge__(self, o): pass
    # def __getattribute__(self, a): pass
    def __getitem__(self, k): pass
    def __gt__(self, o): pass
    def __iadd__(self, o): pass
    def __imul__(self, o): pass
    def __new__(self, o): pass
    def __init__(self, itr): pass
    def __iter__(self): pass
    def __le__(self, o): pass
    def __len__(self): pass
    def __lt__(self, o): pass
    def __mul__(self, o): pass
    def __ne__(self, o): pass
    def __repr__(self, o): pass
    def __rmul__(self, o): pass
    def __setitem__(self, k, v): pass
    def __sizeof__(self, o): pass
    def __reversed__(self): pass
    def append(self): pass
    def clear(self): pass
    def copy(self): pass
    def count(self): pass
    def extend(self): pass
    def index(self): pass
    def insert(self): pass
    def pop(self): pass
    def remove(self): pass
    def reverse(self): pass
    def sort(self): pass


class list_iterator(object):
    def __next__(self): pass
    def __iter__(self): pass

class list_reverseiterator(object):
    def __next__(self): pass
    def __iter__(self): pass


class enumerate(object):
    def __new__(self): pass
    def __next__(self): pass
    def __iter__(self): pass

class zip(object):
    def __new__(self): pass
    def __next__(self): pass
    def __iter__(self): pass

class dict(object):
    def __new__(cls): pass
    def __getitem__(self, k): pass
    def __setitem__(self, k, v): pass
    def __iter__(self): pass
    def __contains__(self, k): pass
    def copy(self): pass
    def clear(self): pass
    def get(self): pass
    def pop(self, key): pass
    def popitem(self): pass
    def keys(self): pass
    def update(self, other): pass
    def values(self): pass
    def items(self): pass

class dict_values(object):
    def __iter__(self): pass

class dict_valueiterator(object):
    def __iter__(self): pass
    def __next__(self): pass

class dict_keys(object):
    def __iter__(self): pass

class dict_keyiterator(object):
    def __iter__(self): pass
    def __next__(self): pass

class dict_items(object):
    def __iter__(self): pass

class dict_itemiterator(object):
    def __iter__(self): pass
    def __next__(self): pass

class range(object):
    def __new__(cls, args): pass
    def __len__(self): pass
    def __iter__(self): pass
    def __contains__(self, v): pass
    def __getitem__(self, k): pass

class range_iterator(object):
    def __next__(self): pass
    def __iter__(self): pass

class set(object):
    def __new__(self): pass
    def __init__(self, start, stop): pass
    def __len__(self): pass
    def __iter__(self): pass
    def __contains__(self, v): pass
    def __eq__(self, o): pass
    def __ge__(self, o): pass
    def __gt__(self, o): pass
    def __le__(self, o): pass
    def __lt__(self, o): pass
    def __ne__(self, o): pass
    def add(self, c): pass
    def clear(self): pass

class set_iterator(object):
    def __next__(self): pass
    def __iter__(self): pass

class tuple(object):
    def __init__(self): pass
    def __iter__(self): pass
    def __contains__(self): pass
    def __getitem__(self, k): pass

class tuple_iterator(object):
    def __next__(self): pass
    def __iter__(self): pass

class slice(object):
    def __new__(self, args): pass

class NotImplementedType(object): pass
class NoneType(object): pass
    # def __eq__(self, other): pass

@mopsa.unsupported
class bytearray(object): pass

class bytes(object):
    def __getitem__(self, i): pass

@mopsa.unsupported
class bytes_iterator(object): pass

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

class reversed(object): pass

@mopsa.unsupported
class staticmethod(object): pass

@mopsa.unsupported
class super(object): pass



##################
##  Exceptions  ##
##################

class BaseException(object):
    def __init__(self, arg): pass
class SystemExit(BaseException): pass
class KeyboardInterrupt(BaseException): pass
class GeneratorExit(BaseException): pass
class Exception(BaseException): pass
class StopIteration(Exception): pass
class StopAsyncIteration(Exception): pass
class ArithmeticError(Exception): pass
class FloatingPointError(ArithmeticError): pass
class OverflowError(ArithmeticError): pass
class ZeroDivisionError(ArithmeticError): pass
class AssertionError(Exception): pass
class AttributeError(Exception): pass
class BufferError(Exception): pass
class EOFError(Exception): pass
class ImportError(Exception): pass
class ModuleNotFoundError(ImportError): pass
class LookupError(Exception): pass
class IndexError(LookupError): pass
class KeyError(LookupError): pass
class MemoryError(Exception): pass
class NameError(Exception): pass
class UnboundLocalError(NameError): pass
class OSError(Exception): pass
class BlockingIOError(OSError): pass
class ChildProcessError(OSError): pass
class ConnectionError(OSError): pass
class BrokenPipeError(ConnectionError): pass
class ConnectionAbortedError(ConnectionError): pass
class ConnectionRefusedError(ConnectionError): pass
class ConnectionResetError(ConnectionError): pass
class FileExistsError(OSError): pass
class FileNotFoundError(OSError): pass
class InterruptedError(OSError): pass
class IsADirectoryError(OSError): pass
class NotADirectoryError(OSError): pass
class PermissionError(OSError): pass
class ProcessLookupError(OSError): pass
class TimeoutError(OSError): pass
class ReferenceError(Exception): pass
class RuntimeError(Exception): pass
class NotImplementedError(RuntimeError): pass
class RecursionError(RuntimeError): pass
class SyntaxError(Exception): pass
class IndentationError(SyntaxError): pass
class TabError(IndentationError): pass
class SystemError(Exception): pass
class TypeError(Exception): pass
class ValueError(Exception): pass
class UnicodeError(ValueError): pass
class UnicodeDecodeError(UnicodeError): pass
class UnicodeEncodeError(UnicodeError): pass
class UnicodeTranslateError(UnicodeError): pass
class Warning(Exception): pass
class DeprecationWarning(Warning): pass
class PendingDeprecationWarning(Warning): pass
class RuntimeWarning(Warning): pass
class SyntaxWarning(Warning): pass
class UserWarning(Warning): pass
class FutureWarning(Warning): pass
class ImportWarning(Warning): pass
class UnicodeWarning(Warning): pass
class BytesWarning(Warning): pass
class ResourceWarning(Warning): pass




#################
##  Functions  ##
#################

def abs(x): pass
def all(itr): pass
def any(itr): pass
def ascii(x): pass
def bin(x): pass
def callable(obj): pass
def chr(i): pass
def compile(src,f,mode): pass
def delattr(obj, attr): pass
def dir(obj): pass
def divmod(a, b): pass
def enumerate(itr, start): pass
def eval(e): pass
def exec(obj): pass
def filter(f, itr): pass
def format(val, fmt): pass
def gettattr(obj, attr): pass
def globals(): pass
def hasattr(obj, attr): pass
def hash(obj): pass
def hex(x): pass
def id(obj): pass
def input(prompt): pass
def isinstance(obj, cls): pass
def issubclass(cls1, cls2): pass
def iter(obj): pass
def len(obj): pass
def locals(): pass
def max(arg1, arg2): pass
def min(arg1, arg2): pass
def next(itr): pass
def oct(x): pass
def open(f): pass
def ord(c): pass
def pow(x, y): pass
def print(obj): pass
def repr(obj): pass
def round(n): pass
def setattr(obj, attr, val): pass
def sorted(itr): pass
def reversed(itr): pass
def sum(itr): pass
def vars(obj): pass
def __import__(modl): pass
