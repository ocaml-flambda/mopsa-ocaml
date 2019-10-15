class overload: pass
Any = object # cheating, removed the ()
class TypeVar: pass

class Union: pass

AnyStr = TypeVar('AnyStr', str, bytes)

# class _SpecialForm:
#     def __getitem__(self, typeargs: Any) -> Any: ...

class Dict: pass
class Tuple: pass
class Callable: pass

class Generic:
    def __new__(self): pass


class Iterable: pass
class Iterator: pass
class IO: pass
class List: pass
class Match: pass
class Optional(Generic[TypeVar('T')]): pass

class Match(Generic[AnyStr]):
    def start(self, group: Union[int, str] = ...) -> int: ...
    def end(self, group: Union[int, str] = ...) -> int: ...
    def span(self, group: Union[int, str] = ...) -> Tuple[int, int]: ...
    def groups(self, default: AnyStr = ...) -> Sequence[AnyStr]: ...

class Pattern(Generic[AnyStr]):
    def search(self, string: AnyStr, pos: int = ..., endpos: int = ...) -> Optional[Match[AnyStr]]: ...

# class Union: pass
class Sequence: pass
class Text: pass
class Mapping: pass
class Literal: pass

class Type: pass
