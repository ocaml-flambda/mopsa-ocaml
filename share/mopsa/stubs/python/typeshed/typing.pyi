class overload: pass
class runtime_checkable: pass
Any = object # cheating, removed the ()
class TypeVar: pass

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

class Protocol(Generic): pass

#Iterator[AnyStr],
class IO(Generic[AnyStr]):
    name : str
    def close(self) -> None: ...
    def read(self, n: int = ...) -> AnyStr: ...
    def write(self, s: AnyStr) -> int: ...

class List: pass
class Set(_Collection[_T_co], Generic[_T_co]):
    # @abstractmethod
    def __contains__(self, x: object) -> bool: ...
    # Mixin methods
    def __le__(self, s: Set[Any]) -> bool: ...
    def __lt__(self, s: Set[Any]) -> bool: ...
    def __gt__(self, s: Set[Any]) -> bool: ...
    def __ge__(self, s: Set[Any]) -> bool: ...
    def __and__(self, s: Set[Any]) -> Set[_T_co]: ...
    def __or__(self, s: Set[_T]) -> Set[Union[_T_co, _T]]: ...
    def __sub__(self, s: Set[Any]) -> Set[_T_co]: ...
    def __xor__(self, s: Set[_T]) -> Set[Union[_T_co, _T]]: ...
    def isdisjoint(self, s: Iterable[Any]) -> bool: ...

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

_T = TypeVar('_T')
_KT = TypeVar('_KT')  # Key type.
_VT = TypeVar('_VT')  # Value type.
_VT_co = TypeVar('_VT_co')#, covariant=True)  # Value type covariant containers.
_T_co = TypeVar('_T_co') # FIXME , covariant=True)  # Any type covariant containers.

class Iterable(Protocol[_T_co]):
    def __iter__(self) -> Iterator[_T_co]: ...

class Iterator(Protocol[_T_co]): #Iterable[_T_co],
# FIXME    @abstractmethod
    def __next__(self) -> _T_co:
        raise StopIteration
    # il faudrait dire qu'on peut raise StopIteration aussi ><'
    def __iter__(self) -> Iterator[_T_co]: ...

@runtime_checkable
class Container(Protocol[_T_co]):
#     @abstractmethod
    def __contains__(self, __x: object) -> bool: ...


@runtime_checkable
class _Collection(Iterable[_T_co], Container[_T_co], Protocol[_T_co]):
    # Implement Sized (but don't have it as a base class).
#     @abstractmethod
    def __len__(self) -> int: ...

@runtime_checkable
class SupportsFloat(Protocol):
#    @abstractmethod
    def __float__(self) -> float: ...


class AbstractSet(_Collection[_T_co], Generic[_T_co]):
#     @abstractmethod
    def __contains__(self, x: object) -> bool: ...
    # Mixin methods
    def __le__(self, s: AbstractSet[Any]) -> bool: ...
    def __lt__(self, s: AbstractSet[Any]) -> bool: ...
    def __gt__(self, s: AbstractSet[Any]) -> bool: ...
    def __ge__(self, s: AbstractSet[Any]) -> bool: ...
    def __and__(self, s: AbstractSet[Any]) -> AbstractSet[_T_co]: ...
    def __or__(self, s: AbstractSet[_T]) -> AbstractSet[Union[_T_co, _T]]: ...
    def __sub__(self, s: AbstractSet[Any]) -> AbstractSet[_T_co]: ...
    def __xor__(self, s: AbstractSet[_T]) -> AbstractSet[Union[_T_co, _T]]: ...
    def isdisjoint(self, s: Iterable[Any]) -> bool: ...

class MappingView:
    def __len__(self) -> int: ...

class ValuesView(MappingView, Iterable[_VT_co], Generic[_VT_co]):
    def __contains__(self, o: object) -> bool: ...
    def __iter__(self) -> Iterator[_VT_co]: ...


# class Union: pass
class Mapping(Generic[_KT, _VT_co], _Collection[_KT]):
    # TODO: We wish the key type could also be covariant, but that doesn't work,
    # see discussion in https: //github.com/python/typing/pull/273.
#     @abstractmethod
    def __getitem__(self, k: _KT) -> _VT_co: ...
        # raise KeyError
    # Mixin methods
    @overload
    def get(self, k: _KT) -> Optional[_VT_co]: ...
    @overload
    def get(self, k: _KT, default: Union[_VT_co, _T]) -> Union[_VT_co, _T]: ...
    def items(self) -> AbstractSet[Tuple[_KT, _VT_co]]: ...
    def keys(self) -> AbstractSet[_KT]: ...
    def values(self) -> ValuesView[_VT_co]: ...
    def __contains__(self, o: object) -> bool: ...

class MutableMapping(Mapping[_KT, _VT], Generic[_KT, _VT]):
#     @abstractmethod
    def __setitem__(self, k: _KT, v: _VT) -> None: ...
#     @abstractmethod
    def __delitem__(self, v: _KT) -> None: ...

    def clear(self) -> None: ...
    @overload
    def pop(self, k: _KT) -> _VT: ...
    @overload
    def pop(self, k: _KT, default: Union[_VT, _T] = ...) -> Union[_VT, _T]: ...
    def popitem(self) -> Tuple[_KT, _VT]: ...
    def setdefault(self, k: _KT, default: _VT = ...) -> _VT: ...
    # 'update' used to take a Union, but using overloading is better.
    # The second overloaded type here is a bit too general, because
    # Mapping[Tuple[_KT, _VT], W] is a subclass of Iterable[Tuple[_KT, _VT]],
    # but will always have the behavior of the first overloaded type
    # at runtime, leading to keys of a mix of types _KT and Tuple[_KT, _VT].
    # We don't currently have any way of forcing all Mappings to use
    # the first overload, but by using overloading rather than a Union,
    # mypy will commit to using the first overload when the argument is
    # known to be a Mapping with unknown type parameters, which is closer
    # to the behavior we want. See mypy issue  #1430.
    @overload
    def update(self, __m: Mapping[_KT, _VT]) -> None: ... #, **kwargs: _VT
    @overload
    def update(self, __m: Iterable[Tuple[_KT, _VT]]) -> None: ... # , **kwargs: _VT
    # @overload
    # def update(self, **kwargs: _VT) -> None: ...


class Literal: pass

class Type: pass


# cheating, the next 3 should be in types.pyi but we have some recursive import I don't want to handle for now
class _CodeType:
#    """Create a code object.  Not for the faint of heart."""
    co_argcount: int
    co_kwonlyargcount: int
    co_nlocals: int
    co_stacksize: int
    co_flags: int
    co_code: bytes
    co_consts: Tuple[Any, ...]
    co_names: Tuple[str, ...]
    co_varnames: Tuple[str, ...]
    co_filename: str
    co_name: str
    co_firstlineno: int
    co_lnotab: bytes
    co_freevars: Tuple[str, ...]
    co_cellvars: Tuple[str, ...]
    def __init__(
        self,
        argcount: int,
        kwonlyargcount: int,
        nlocals: int,
        stacksize: int,
        flags: int,
        codestring: bytes,
        constants: Tuple[Any, ...],
        names: Tuple[str, ...],
        varnames: Tuple[str, ...],
        filename: str,
        name: str,
        firstlineno: int,
        lnotab: bytes,
        freevars: Tuple[str, ...] = ...,
        cellvars: Tuple[str, ...] = ...,
    ) -> None: ...


class _FrameType:
    f_back: _FrameType
    f_builtins: Dict[str, Any]
    f_code: _CodeType
    f_globals: Dict[str, Any]
    f_lasti: int
    f_lineno: int
    f_locals: Dict[str, Any]
    f_trace: Callable[[], None]
#    if sys.version_info >= (3, 7):
    f_trace_lines: bool
    f_trace_opcodes: bool

    def clear(self) -> None: ...

class _TracebackType:
    def __init__(self, tb_next: Optional[_TracebackType], tb_frame: _FrameType, tb_lasti: int, tb_lineno: int) -> None: ...
    tb_next: Optional[_TracebackType]
    # the rest are read-only even in 3.7
    @property
    def tb_frame(self) -> _FrameType: ...
    @property
    def tb_lasti(self) -> int: ...
    @property
    def tb_lineno(self) -> int: ...


@runtime_checkable
class ContextManager(Protocol[_T_co]):
    def __enter__(self) -> _T_co: ...
    def __exit__(self, __exc_type: Optional[Type[BaseException]],
                 __exc_value: Optional[BaseException],
                 __traceback: Optional[_TracebackType]) -> Optional[bool]: ...
