class overload: pass
class runtime_checkable: pass
Any = object # cheating, removed the ()
class TypeVar: pass

class Protocol: pass
class Union: pass

AnyStr = TypeVar('AnyStr', str, bytes)
Text = str
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
class Sequence(Generic[TypeVar('T')]):
    def __getitem__(self, i: int) -> TypeVar('T'): ...

class Match(Generic[AnyStr]):
    def start(self, group: Union[int, str] = ...) -> int: ...
    def end(self, group: Union[int, str] = ...) -> int: ...
    def span(self, group: Union[int, str] = ...) -> Tuple[int, int]: ...
    def groups(self, default: AnyStr = ...) -> Sequence[AnyStr]: ...
    def group(self) -> AnyStr: ...

class Pattern(Generic[AnyStr]):
    def search(self, string: AnyStr, pos: int = ..., endpos: int = ...) -> Optional[Match[AnyStr]]: ...

# class Union: pass
class Mapping: pass
class Literal: pass

class Type: pass

_T_co = TypeVar('_T_co') # FIXME , covariant=True)  # Any type covariant containers.
class Iterable(Protocol[_T_co]):
    def __iter__(self) -> Iterator[_T_co]: ...

class Iterator(Iterable[_T_co], Protocol[_T_co]):
# FIXME    @abstractmethod
    def __next__(self) -> _T_co: ...
    def __iter__(self) -> Iterator[_T_co]: ...
