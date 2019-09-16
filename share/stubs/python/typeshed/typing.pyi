class overload: pass
Any = object # cheating, removed the ()
TypeVar = object()

class Union: pass

AnyStr = TypeVar('AnyStr', str, bytes)

# class _SpecialForm:
#     def __getitem__(self, typeargs: Any) -> Any: ...

class Tuple: pass
class Callable: pass


class Iterable: pass
class Iterator: pass
class IO: pass
class List: pass
class Match: pass
class Optional: pass
class Pattern: pass
# class Union: pass
class Sequence: pass
class Text: pass
class Mapping: pass
class Literal: pass
class Generic: pass

class Type: pass
