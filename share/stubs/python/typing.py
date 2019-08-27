overload = object()
Any = object()
TypeVar = object()

# AnyStr = TypeVar('AnyStr', str, bytes)

class _SpecialForm:
    def __getitem__(self, typeargs: Any) -> Any: ...

Tuple: _SpecialForm
Callable: _SpecialForm


class Iterable: pass
class Iterator: pass
class IO: pass
class List: pass
class Match: pass
class Optional: pass
class Pattern: pass
class Union: pass
class Sequence: pass
class Text: pass
class Mapping: pass
class Literal: pass
class Generic: pass

class Type: pass
