(**

  Copyright (c) 2017-2019 Aymeric Fromherz and The MOPSA Project

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 *)

let functions = [
  (* Functions *)
  "abs";
  "all";
  "any";
  "ascii";
  "bin";
  "callable";
  "chr";
  "classmethod";
  "compile";
  "delattr";
  "dir";
  "divmod";
  "enumerate";
  "eval";
  "exec";
  "filter";
  "format";
  "getattr";
  "globals";
  "hasattr";
  "hash";
  "help";
  "hex";
  "id";
  "input";
  "isinstance";
  "issubclass";
  "iter";
  "len";
  "locals";
  "map";
  "max";
  "memoryview";
  "min";
  "next";
  "oct";
  "open";
  "ord";
  "pow";
  "print";
  "repr";
  "reversed";
  "round";
  "setattr";
  "sorted";
  "staticmethod";
  "sum";
  "super";
  "vars";
  "zip";
  "__import__"
]

(* Exceptions *)
let  exceptions = [
  "BaseException";
  "Exception";
  "AssertionError";
  "AttributeError";
  "StopIteration";
  "SyntaxError";
  "TypeError";
  "ValueError";
  "NameError";
  "UnboundLocalError";
  "LookupError";
  "IndexError";
  "RuntimeError";
  "KeyError";
  "MemoryError";
  "OverflowError";
  "ZeroDivisionError";
  "DeprecationWarning";
  "UnicodeEncodeError";
  "RecursionError";
  "SystemError";
  "BufferError";
  "ArithmeticError";
  "EOFError";
  "FloatingPointError";
  "GeneratorExit";
  "ImportError";
  "ModuleNotFoundError";
  "KeyboardInterrupt";
  "NotImplementedError";
  "OSError";
  "ReferenceError";
  "StopAsyncIteration";
  "IndentationError";
  "TabError";
  "SystemExit";
  "UnicodeError";
  "UnicodeDecodeError";
  "UnicodeTranslateError";
  "EnvironmentError";
  "IOError";
  "WindowsError";
  "BlockingIOError";
  "ChildProcessError";
  "ConnectionError";
  "BrokenPipeError";
  "ConnectionAbortedError";
  "ConnectionRefusedError";
  "ConnectionResetError";
  "FileExistsError";
  "FileNotFoundError";
  "InterruptedError";
  "IsADirectoryError";
  "NotADirectoryError";
  "PermissionError";
  "ProcessLookupError";
  "TimeoutError";
  "Warning";
  "UserWarning";
  "PendingDeprecationWarning";
  "SyntaxWarning";
  "RuntimeWarning";
  "FutureWarning";
  "ImportWarning";
  "UnicodeWarning";
  "BytesWarning";
  "ResourceWarning";
]

  (* Classes *)
let classes = [
  "bool";
  "bytearray";
  "bytes";
  "complex";
  "dict";
  "float";
  "frozenset";
  "int";
  "list";
  "listiter";
  "object";
  "property";
  "range";
  "rangeiter";
  "set";
  "slice";
  "str";
  "tuple";
  "tupleiter";
  "type";
  "xrange"
]

let vars = []
(*   "__name__";
 *   "__file__"
 * ] *)

let decorators = [
  "trackCall";
  "classmethod";
  "staticmethod"
]

let all = functions @ exceptions @ classes @ vars @ decorators
