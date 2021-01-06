#include <Python.h>

int PyType_Ready(PyTypeObject *type)
{
    Py_TYPE(type) = &PyType_Type;
    return 0;
}
