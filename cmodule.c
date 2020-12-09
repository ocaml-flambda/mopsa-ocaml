#include <Python.h>
#include "structmember.h"

// two stubs used by the analysis
// FIXME: put them in a separate file and handle its import
int PyType_ReadyCheat(PyTypeObject *type)
{
    Py_TYPE(type) = &PyType_Type;
    type->tp_alloc = PyType_GenericAlloc;
//    _mopsa_print();
    return 0;
}

PyObject*
PyType_GenericAlloc(PyTypeObject *type, Py_ssize_t nitems)
{
    PyObject *obj;
    const size_t size = _PyObject_VAR_SIZE(type, nitems+1);

    obj = PyType_GenericAlloc_Helper(type, size);
    memset(obj, '\0', size);

    /* FIXME */
    if (obj == NULL)
        return PyErr_NoMemory();

//    _mopsa_print();
    return obj;
}
// end of stubs

typedef struct {
    PyObject_HEAD
    PyObject* contents;
} Cbox;

static PyObject*
Cbox_new(PyTypeObject *type, PyObject *args1, PyObject *kwds)
{
    Cbox *self;
    self = (Cbox *) type->tp_alloc(type, 0);
    /* FIXME: next line should be in tp_alloc */
    Py_TYPE(self) = type;
//    _mopsa_print("bla");
    if (self != NULL) {
        self->contents = NULL;
    }

    return (PyObject *) self;
}

static int
Cbox_init(Cbox *self, PyObject *args2, PyObject *kwds)
{
    PyObject *c;
    if(!PyArg_ParseTuple(args2, "O", &c))
        return -1;// 0 ~> assertion fails + coredump

    if(c)
        self->contents = c;

//    _mopsa_print();
    return 0; // -1 ~> coredump o/
}

static PyObject *
Cbox_getcontents(Cbox *self, PyObject *args)
{
    return self->contents;
}

static PyMethodDef Cbox_methods[] = {
    {"getcontents", (PyCFunction) Cbox_getcontents, METH_VARARGS, ""},
    {NULL}  /* Sentinel */
};

static PyMemberDef Cbox_members[] = {
    {"contents", T_OBJECT, offsetof(Cbox, contents), 0, "contents"},
    {NULL}  /* Sentinel */
};

static PyTypeObject CboxType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "c.Cbox",
    .tp_doc = "Custom C Box",
    .tp_basicsize = sizeof(Cbox),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /* If you don't have Py_TPFLAGS_BASETYPE, you can't subclass! */
    .tp_new = Cbox_new,
    .tp_init = Cbox_init,
    .tp_members = Cbox_members,
    .tp_methods = Cbox_methods
};

static PyObject*
c_typ(PyObject *self, PyObject *args)
{
    printf("%s", PyModule_GetName(self));
    return (PyObject*) Py_TYPE(&CboxType);
}

static PyObject*
c_broken(PyObject *self, PyObject *args)
{
    return 42;
}


static PyMethodDef module_methods[] = {
    {"typ", (PyCFunction) c_typ, METH_VARARGS, ""},
    {"broken", (PyCFunction) c_broken, METH_VARARGS, ""},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef cmodule = {
    PyModuleDef_HEAD_INIT,
    "c",
    NULL,
    -1,
    module_methods
};


PyMODINIT_FUNC
PyInit_c(void) // need to define PyInit_c rather than _bla
{
    printf("PyInit_c\n");
    PyObject *m;
    if (PyType_Ready(&CboxType)) return NULL;
    m = PyModule_Create(&cmodule);
    if (m == NULL) return NULL;
    Py_INCREF(&CboxType);
    if (PyModule_AddObject(m, "Cbox", (PyObject *) &CboxType) < 0) {
        Py_DECREF(&CboxType);
        Py_DECREF(m);
        return NULL;
    }
    return m;
}
