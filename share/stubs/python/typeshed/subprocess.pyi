from typing import Generic, AnyStr, Sequence, Text, Union, IO, overload, Optional, Any, Callable, Mapping, Literal, Tuple, List

_FILE = Union[None, int, IO[Any]]
_TXT = Union[bytes, Text]
_PATH = Union[bytes, Text] # _PathLike if py >= 3.6...
# _CMD = Union[_TXT, Sequence[_PATH]]
_CMD = List[str]
_ENV = Union[Mapping[bytes, _TXT], Mapping[Text, _TXT]]
PIPE: int

class Popen(Generic[AnyStr]):
    args: _CMD
    stdin: IO[AnyStr]
    stdout: IO[AnyStr]
    stderr: IO[AnyStr]
    pid = 0
    returncode = 0
    @overload
    def __new__(cls,
                args: _CMD,
                stdin: Optional[_FILE] = ...,
                stdout: Optional[_FILE] = ...,
                stderr: Optional[_FILE] = ...,
                shell: bool = ...,
                universal_newlines: Literal[True]) -> Popen[str]: ...
    def __init__(cls,
                args: _CMD,
                stdin: Optional[_FILE] = ...,
                stdout: Optional[_FILE] = ...,
                stderr: Optional[_FILE] = ...,
                shell: bool = ...,
                universal_newlines: Literal[True]) -> None: ...
    # @overload
    # def __new__(cls,
    #             args: _CMD,
    #             bufsize: int = ...,
    #             executable: Optional[_PATH] = ...,
    #             stdin: Optional[_FILE] = ...,
    #             stdout: Optional[_FILE] = ...,
    #             stderr: Optional[_FILE] = ...,
    #             preexec_fn: Optional[Callable[[], Any]] = ...,
    #             close_fds: bool = ...,
    #             shell: bool = ...,
    #             cwd: Optional[_PATH] = ...,
    #             env: Optional[_ENV] = ...,
    #             # *,
    #             universal_newlines: Literal[False] = ...,
    #             startupinfo: Optional[Any] = ...,
    #             creationflags: int = ...,
    #             restore_signals: bool = ...,
    #             start_new_session: bool = ...,
    #             pass_fds: Any = ...) -> Popen[bytes]: ...
    # @overload
    # def __new__(cls,
    #             args: _CMD,
    #             bufsize: int = ...,
    #             executable: Optional[_PATH] = ...,
    #             stdin: Optional[_FILE] = ...,
    #             stdout: Optional[_FILE] = ...,
    #             stderr: Optional[_FILE] = ...,
    #             preexec_fn: Optional[Callable[[], Any]] = ...,
    #             close_fds: bool = ...,
    #             shell: bool = ...,
    #             cwd: Optional[_PATH] = ...,
    #             env: Optional[_ENV] = ...,
    #             universal_newlines: bool = ...,
    #             startupinfo: Optional[Any] = ...,
    #             creationflags: int = ...,
    #             restore_signals: bool = ...,
    #             start_new_session: bool = ...,
    #             pass_fds: Any = ...) -> Popen[Any]: ...
    # Return str/bytes
    def communicate(self,
                    input: Optional[AnyStr] = ...,
                    timeout: Optional[float] = ...,
                    # morally this should be optional
                    ) -> Tuple[AnyStr, AnyStr]: ...
