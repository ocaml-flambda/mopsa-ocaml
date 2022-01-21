#ifndef CPYTHON_STUBS_SEEN
#define CPYTHON_STUBS_SEEN

#include <Python.h>
#include <structmember.h>

// transfer function in cmodule.c
PyObject *PyUnicode_GetItem(PyObject *list, Py_ssize_t index);

static inline void _noop(PyObject *op) {}

#undef Py_INCREF
#undef Py_DECREF
#undef Py_XINCREF
#undef Py_XDECREF
#undef Py_CLEAR
#define Py_INCREF(op) _noop(_PyObject_CAST(op))
#define Py_DECREF(op) _noop(_PyObject_CAST(op))
#define Py_XINCREF(op) _noop(_PyObject_CAST(op))
#define Py_XDECREF(op) _noop(_PyObject_CAST(op))
#define Py_CLEAR(op) _noop(_PyObject_CAST(op))

#undef PyArg_ParseTuple
#undef PyArg_ParseTupleAndKeywords
#undef Py_BuildValue
#undef PyObject_CallFunction

// FIXME: add other builtins like that
#undef PyUnicode_GET_LENGTH
#undef PyUnicode_GET_SIZE
#undef PyTuple_GET_SIZE
#undef PyTuple_GET_ITEM
#undef PyTuple_SET_ITEM
#define PyUnicode_GET_LENGTH PyUnicode_GetLength
#define PyUnicode_GET_SIZE PyUnicode_GetLength
#define PyTuple_GET_SIZE PyTuple_Size
#define PyTuple_GET_ITEM PyTuple_GetItem
#define PyTuple_SET_ITEM PyTuple_SetItem
#undef PyUnicode_AS_UNICODE
#define PyUnicode_AS_UNICODE PyUnicode_AsUnicode
#undef Py_UNICODE_IS_SURROGATE
#undef Py_UNICODE_IS_LOW_SURROGATE
#undef Py_UNICODE_IS_HIGH_SURROGATE
#undef PyString_GET_SIZE

#undef PyBytes_GET_SIZE
#define PyBytes_GET_SIZE PyBytes_Size
#undef PyBytes_AS_STRING
#define PyBytes_AS_STRING PyBytes_AsString

#undef PyList_GET_SIZE
#define PyList_GET_SIZE PyList_Size
#undef PyList_GET_ITEM
#define PyList_GET_ITEM PyList_GetItem
#undef PyList_SET_ITEM
#define PyList_SET_ITEM PyList_SetItem


#undef PyFloat_AS_DOUBLE
#define PyFloat_AS_DOUBLE PyFloat_AsDouble

#define _PyObject_GC_New _PyObject_New


/* // stubs used by the analysis */
typedef struct exc_data {
    PyObject* exc_state;
    char* exc_msg;
} exc_data;

int _mopsa_pyerr_bind_cs_to(exc_data*);

exc_data *exc;
exc_data no_exc = {NULL, NULL};

#define PyErr_Occurred() exc->exc_state
#define PyErr_GetMsg() exc->exc_msg
#undef PyErr_BadInternalCall
void PyErr_BadInternalCall(void);


int PyType_ReadyCheat(PyTypeObject *type);

void
_PyType_Assign_Helper(PyObject* obj, PyTypeObject* type);

void
init_flags();

void set_tp_alloc_py_class(PyTypeObject *t, PyTypeObject *base);
void set_default_flags(PyTypeObject *t);

int PyParseTuple_int_helper(PyObject *obj, int *result);
int PyParseTuple_shortint_helper(PyObject *obj, short *result);

#define call_method PyObject_CallMethod
#undef PyObject_CallMethod
PyObject *
PyObject_CallMethod(PyObject *obj, const char *name, const char *format, ...);

#endif
