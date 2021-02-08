#include <Python.h>
#include "structmember.h"

#include "python_stubs.c"

// the Cbox objects has two C fields:
// - contents, which can be any python object
// - counter, a C integer
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

    PyObject* l = PyTuple_GetItem(args1, 1);
    if(PyLong_AsLong(l) == -1)
        return NULL;

    return (PyObject *) self;
}

static int
Cbox_init(Cbox *self, PyObject *args2, PyObject *kwds)
{
    PyObject *c;
    int d;
    if(!PyArg_ParseTuple(args2, "Oi", &c, &d))
    // FIXME: si i~>O, on devrait dire qu'il y a un soucis
        return -1;

    if(c)
        self->contents = c;

    if(PyUnicode_Check(c))
        d = PyUnicode_GetLength(c);

    if(d)
        self->counter = d;
    return 0; // -1 ~> coredump o/
}

static PyObject *
Cbox_getcontents(Cbox *self, PyObject *args)
{
    PyObject* res = self->contents;
    return res;
}

static PyObject*
Cbox_getcounter(Cbox *self, PyObject *args)
{
    PyObject* res = Py_BuildValue("i", self->counter); // self->contents
    return res;
}

static PyObject *
Cbox_incr(Cbox *self, PyObject *args)
{
    self->counter++;
    Py_RETURN_NONE;
}


static PyObject*
Cbox_maybe_incr(Cbox *self, PyObject *args)
{
    if(_mopsa_rand_s8()) {
        return NULL;
    }
    self->counter++;
    Py_RETURN_NONE;
}

static PyObject*
Cbox_maybe_incr2(Cbox *self, PyObject *args)
{
    PyObject* a = Cbox_maybe_incr(self, args);
    return a;
}



static PyMethodDef Cbox_methods[] = {
    {"getcontents", (PyCFunction) Cbox_getcontents, METH_VARARGS, ""},
    {"getcounter", (PyCFunction) Cbox_getcounter, METH_VARARGS, ""},
    {"incr", (PyCFunction) Cbox_incr, METH_VARARGS, ""},
    {"maybe_incr", (PyCFunction) Cbox_maybe_incr, METH_VARARGS, ""},
    {"maybe_incr2", (PyCFunction) Cbox_maybe_incr2, METH_VARARGS, ""},
    {NULL}  /* Sentinel */
};

static PyMemberDef Cbox_members[] = {
    {"counter", T_INT, offsetof(Cbox, counter), 0, "counter doc"},
    {"contents", T_OBJECT, offsetof(Cbox, contents), READONLY, "contents doc"},
    {NULL}  /* Sentinel */
};

static PyTypeObject CboxType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "basic.Cbox",
    .tp_doc = "Custom C Box",
    .tp_basicsize = sizeof(Cbox),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = Cbox_new,
    .tp_init = Cbox_init,
    .tp_members = Cbox_members,
    .tp_methods = Cbox_methods
};


typedef struct {
    PyObject_HEAD
    int counter;
} Counter;

static Py_ssize_t
Counter_len(PyObject* self)
{
    Py_ssize_t ret = ((Counter*) self)->counter;
    // FIXME: what happens if counter_len returns a negative result?
    if(ret == -1)
        PyErr_SetString(PyExc_TypeError, "uninitiliazed");
    return ret;
}

static int
Counter_contains(PyObject* self, PyObject* args)
{
    int c = PyLong_AsLong(args);
    if(c == -1 && PyErr_Occurred())
        return -1;

    return c == ((Counter*) self)->counter;
}

static int
Counter_init(Counter *self, PyObject *args, PyObject *kwds)
{
    PyObject *c;
    if(!PyArg_ParseTuple(args, "O", &c))
        return -1;

    if(c && PyLong_Check(c))
    {
        self->counter = PyLong_AsSsize_t(c);
        return 0;
    }
    else
    {
        PyErr_SetString(PyExc_TypeError, "integer required");
        // FIXME: what happens if no exc set
        return -1;
    }
}

static PySequenceMethods counter_as_sequence = {
    (lenfunc)Counter_len,                 /* sq_length */
    0,                                  /* sq_concat */
    0,                                  /* sq_repeat */
    0,                                  /* sq_item */
    0,                                  /* sq_slice */
    0,                                  /* sq_ass_item */
    0,                                  /* sq_ass_slice */
    Counter_contains,                    /* sq_contains */
    0,                                  /* sq_inplace_concat */
    0,                                  /* sq_inplace_repeat */
};


static PyTypeObject CounterType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "basic.Counter",
    .tp_doc = "bla",
    .tp_basicsize = sizeof(Counter),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = PyType_GenericNew,
    .tp_init = Counter_init,
    .tp_as_sequence = &counter_as_sequence
};

static PyObject*
basic_return_true(PyObject *self, PyObject *args)
{
    Py_RETURN_TRUE;
}

static PyObject*
basic_return_false(PyObject *self, PyObject *args)
{
    Py_RETURN_FALSE;
}

static PyObject*
basic_return_bool(PyObject *self, PyObject *args)
{
    PyObject* r;
    if(_mopsa_rand_s8()) { Py_RETURN_TRUE; }
    else { Py_RETURN_FALSE; }
}

static PyObject*
basic_return_none(PyObject *self, PyObject *args)
{
    Py_RETURN_NONE;
}

static PyObject*
basic_typ(PyObject *self, PyObject *args)
{
    PyObject* a;
    if(!PyArg_ParseTuple(args, "O", &a))
        return NULL;
    return (PyObject*) Py_TYPE(a);
}

static PyObject*
basic_raise_exc(PyObject *self, PyObject *args)
{
    PyErr_SetString(PyExc_AttributeError, "blaaa");
    return NULL;
}


static PyObject*
basic_forget_raise(PyObject *self, PyObject *args)
{
    return NULL;
}

static PyObject*
basic_id_check(PyObject *self, PyObject *args)
{
    if(PyTuple_Size(args) != 1)
    {
        PyErr_SetString(PyExc_TypeError, "one argument expected");
        return NULL;
    }
    return PyTuple_GetItem(args, 0);
}

static PyObject*
basic_random_fail(PyObject *self, PyObject *args)
{
    PyObject* r;
    if(_mopsa_rand_s8()) { r = basic_id_check(self, args); }
    else { r = basic_raise_exc(self, args); }
    return r;
}


static PyMethodDef module_methods[] = {
    {"typ", (PyCFunction) basic_typ, METH_VARARGS, ""},
    {"raise_exc", (PyCFunction) basic_raise_exc, METH_VARARGS, ""},
    {"forget_raise", (PyCFunction) basic_forget_raise, METH_VARARGS, ""},
    {"id_check", (PyCFunction) basic_id_check, METH_VARARGS, ""},
    {"random_fail", (PyCFunction) basic_random_fail, METH_VARARGS, ""},
    {"return_true", (PyCFunction) basic_return_true, METH_VARARGS, ""},
    {"return_false", (PyCFunction) basic_return_false, METH_VARARGS, ""},
    {"return_bool", (PyCFunction) basic_return_bool, METH_VARARGS, ""},
    {"return_none", (PyCFunction) basic_return_none, METH_VARARGS, ""},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef basicmodule = {
    PyModuleDef_HEAD_INIT,
    "basic",
    NULL,
    -1,
    module_methods
};


PyMODINIT_FUNC
PyInit_basic(void) // need to define PyInit_c rather than _bla
{
    PyObject *m;
    if (PyType_Ready(&CboxType)) return NULL;
    if (PyType_Ready(&CounterType)) return NULL;
    m = PyModule_Create(&basicmodule);
    if (m == NULL) return NULL;
    Py_INCREF(&CboxType);
    if (PyModule_AddObject(m, "Cbox", (PyObject *) &CboxType) < 0) {
        Py_DECREF(&CboxType);
        Py_DECREF(m);
        return NULL;
    }
    Py_INCREF(&CounterType);
    if (PyModule_AddObject(m, "Counter", (PyObject *) &CounterType) < 0) {
        Py_DECREF(&CounterType);
        Py_DECREF(m);
        return NULL;
    }
    PyModule_AddIntConstant(m, "version", 0);
    return m;
}
