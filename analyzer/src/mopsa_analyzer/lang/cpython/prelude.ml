open Mopsa
open Python.Addr

let builtin_exceptions =
  [
    "PyExc_BaseException";
    "PyExc_Exception";
    "PyExc_StopAsyncIteration";
    "PyExc_StopIteration";
    "PyExc_GeneratorExit";
    "PyExc_ArithmeticError";
    "PyExc_LookupError";
    "PyExc_AssertionError";
    "PyExc_AttributeError";
    "PyExc_BufferError";
    "PyExc_EOFError";
    "PyExc_FloatingPointError";
    "PyExc_OSError";
    "PyExc_ImportError";
    "PyExc_ModuleNotFoundError";
    "PyExc_IndexError";
    "PyExc_KeyError";
    "PyExc_KeyboardInterrupt";
    "PyExc_MemoryError";
    "PyExc_NameError";
    "PyExc_OverflowError";
    "PyExc_RuntimeError";
    "PyExc_RecursionError";
    "PyExc_NotImplementedError";
    "PyExc_SyntaxError";
    "PyExc_IndentationError";
    "PyExc_TabError";
    "PyExc_ReferenceError";
    "PyExc_SystemError";
    "PyExc_SystemExit";
    "PyExc_TypeError";
    "PyExc_UnboundLocalError";
    "PyExc_UnicodeError";
    "PyExc_UnicodeEncodeError";
    "PyExc_UnicodeDecodeError";
    "PyExc_UnicodeTranslateError";
    "PyExc_ValueError";
    "PyExc_ZeroDivisionError";
    "PyExc_BlockingIOError";
    "PyExc_BrokenPipeError";
    "PyExc_ChildProcessError";
    "PyExc_ConnectionError";
    "PyExc_ConnectionAbortedError";
    "PyExc_ConnectionRefusedError";
    "PyExc_ConnectionResetError";
    "PyExc_FileExistsError";
    "PyExc_FileNotFoundError";
    "PyExc_InterruptedError";
    "PyExc_IsADirectoryError";
    "PyExc_NotADirectoryError";
    "PyExc_PermissionError";
    "PyExc_ProcessLookupError";
    "PyExc_TimeoutError";
    "PyExc_EnvironmentError";
    "PyExc_IOError";
    "PyExc_Warning";
    "PyExc_UserWarning";
    "PyExc_DeprecationWarning";
    "PyExc_PendingDeprecationWarning";
    "PyExc_SyntaxWarning";
    "PyExc_RuntimeWarning";
    "PyExc_FutureWarning";
    "PyExc_ImportWarning";
    "PyExc_UnicodeWarning";
    "PyExc_BytesWarning";
    "PyExc_ResourceWarning";
  ]

let () =
  C.Cstubs.Resources.register_is_resource_addr_chain (fun next ak ->
      match ak with
      (* FIXME: other container addresses *)
      | Python.Objects.Py_list.A_py_list
      | Python.Objects.Py_list.A_py_iterator _
      | Python.Objects.Py_set.A_py_set
      | Python.Objects.Tuple.A_py_tuple _
      | Python.Objects.Dict.A_py_dict
      | A_py_instance _
      | A_py_class _
      | A_py_c_class _
      | A_py_c_function _
      | A_py_c_module _
      | A_py_function _
      | A_py_method _
      | A_py_module _ -> true
      | _ -> next ak)

let () =
  C.Common.Base.register_addr_opaque (fun next ->
      function
      | Python.Objects.Py_list.A_py_list
      | Python.Objects.Py_list.A_py_iterator _
      | Python.Objects.Py_set.A_py_set
      | Python.Objects.Tuple.A_py_tuple _
      | Python.Objects.Dict.A_py_dict
      | A_py_instance {addr_kind = A_py_class (C_builtin _, _)} -> OpaqueFrom 8
      | ak -> next ak)



let builtin_functions =
          [
          "PyModule_Create2";
          "PyModule_AddObject";
          "PyType_FromSpec";
          "PyType_Ready";
          "PyType_GenericAlloc_Helper";
          "PyType_IsSubtype";
          "PyArg_ParseTuple";
          "PyArg_ParseTupleAndKeywords";
          "PyArg_UnpackTuple";
          "Py_BuildValue";
          "PyNumber_Add";
          "PyObject_CallFunction";
          "PyObject_CallObject";
          "PyObject_CallMethod";
          "PyObject_GetAttrString";
          "PyObject_GetItem";
          "PyObject_GetIter";
          "PyObject_RichCompare";
          "PyObject_RichCompareBool";
          "PyObject_Size";
          "PyObject_Repr";
          "PyObject_Length";
          "PyObject_IsTrue";
          "PyIter_Next";
          "PySequence_GetSlice";
          "PyLong_FromLong";
          "PyLong_FromUnsignedLong";
          "PyLong_FromSsize_t";
          "PyLong_AsLong";
          "PyLong_AsSsize_t";
          "PyFloat_FromDouble";
          "PyFloat_AsDouble";
          "PyBytes_FromStringAndSize";
          "PyBytes_Size";
          "PyBytes_AsString";
          "PyUnicode_Concat";
          "PyUnicode_GetLength";
          "PyUnicode_InternFromString";
          "PyUnicode_FromString";
          "PyUnicode_FromKindAndData";
          "PyUnicode_FromWideChar";
          "PyUnicode_AsEncodedString";
          "PyUnicode_AsUnicode";
          "PyUnicode_AsUTF8AndSize";
          "PyTuple_New";
          "PyTuple_SetItem";
          "PyTuple_Size";
          "PyTuple_GetItem";
          "PyTuple_GetSlice";
          "PyList_New";
          "PyList_Size";
          "PyList_GetItem";
          "PyList_SetItem";
          "PyList_Append";
          "PyDict_Size";
          "PyDict_Next";
          "PyDict_New";
          "PyDict_GetItem";
          "PyDict_SetItem";
          "PySet_New";
          "PySet_Size";
          "PySet_Add";
          "PySet_Clear";
          "PyWeakref_NewRef";
          "PyWeakref_GetObject";
        ]
