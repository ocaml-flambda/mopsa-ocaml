#include <Python.h>
#include "structmember.h"

#include "python_stubs.c"

typedef struct {
    PyObject_HEAD
    PyObject* contents;
    int counter;
} Cbox;

static PyObject*
Cbox_new(PyTypeObject *type, PyObject *args1, PyObject *kwds)
{
    Cbox *self;
    self = (Cbox *) type->tp_alloc(type, 0);
    if (self != NULL) {
        self->contents = NULL;
        self->counter = 0;
    }

    return (PyObject *) self;
}

static int
Cbox_init(Cbox *self, PyObject *args2, PyObject *kwds)
{
    PyObject *c;
    int *d;
    if(!PyArg_ParseTuple(args2, "OO", &c, &d))
        return -1;// 0 ~> assertion fails + coredump

    if(c)
        self->contents = c;
    if(d)
        self->counter = d;
    assert (PyUnicode_Check(d));
//    _mopsa_print();
    return 0; // -1 ~> coredump o/
}

static PyObject *
Cbox_getcontents(Cbox *self, PyObject *args)
{
    PyObject* res = self->contents;
    return res;
}

static PyObject *
Cbox_incr(Cbox *self, PyObject *args)
{
    self->counter++;
    Py_RETURN_NONE;
}


static PyMethodDef Cbox_methods[] = {
    {"getcontents", (PyCFunction) Cbox_getcontents, METH_VARARGS, ""},
    {"incr", (PyCFunction) Cbox_incr, METH_VARARGS, ""},
    {NULL}  /* Sentinel */
};

static PyMemberDef Cbox_members[] = {
    {"counter", T_INT, offsetof(Cbox, counter), 0, "counter doc"},
    {"contents", T_OBJECT, offsetof(Cbox, contents), READONLY, "contents doc"},
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
    PyObject* a;
    if(!PyArg_ParseTuple(args, "O", &a))
        return -1;// 0 ~> assertion fails + coredump
    return (PyObject*) Py_TYPE(a);
}

static PyObject*
c_broken(PyObject *self, PyObject *args)
{
    PyErr_SetString(PyExc_AttributeError, "blaaa");
    return NULL;
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
