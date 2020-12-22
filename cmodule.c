#include <Python.h>
#include "structmember.h"

// stubs used by the analysis
// FIXME: put them in a separate file and handle its import
PyObject* exc_state = NULL;
char* exc_msg = NULL;

PyObject*
PyErr_NoMemory()
{
    exc_state = PyExc_MemoryError;
//    _mopsa_print();
    return NULL;
}

PyObject*
PyErr_Occured()
{
    return exc_state;
};

void PyErr_SetString(PyObject* exc, const char* msg){
    exc_state = exc;
    exc_msg = msg;
//    _mopsa_print();
}


int PyType_ReadyCheat(PyTypeObject *type)
{
    Py_TYPE(type) = &PyType_Type;
    type->tp_alloc = PyType_GenericAlloc;
    return 0;
}

PyObject*
PyType_GenericAlloc(PyTypeObject *type, Py_ssize_t nitems)
{
    PyObject *obj;
    const size_t size = _PyObject_VAR_SIZE(type, nitems+1);

    obj = PyType_GenericAlloc_Helper(type, size);
    memset(obj, '\0', size);
    Py_TYPE(obj) = type;

    /* FIXME */
    if (obj == NULL)
        return PyErr_NoMemory();

    return obj;
}

static PyObject *
// remove member_get and directly call PyMember_GetOne?
// was PyMemberDescrObject, we're cheating again
member_get(PyMemberDef *descr, PyObject *obj, PyObject *type)
{
    /* PyObject *res; */

    /* if (descr_check((PyDescrObject *)descr, obj, &res)) */
    /*     return res; */

    /* if (descr->d_member->flags & READ_RESTRICTED) { */
    /*     if (PySys_Audit("object.__getattr__", "Os", */
    /*         obj ? obj : Py_None, descr->d_member->name) < 0) { */
    /*         return NULL; */
    /*     } */
    /* } */
    return PyMember_GetOne((char *)obj, descr); // was descr->d_member, we're cheating
}

// FIXME: we want to check the flag at creation rather than first call
// FIXME: simplified version, check the real one in Python/structmember.c
PyObject *
PyMember_GetOne(const char *addr, PyMemberDef *l)
{
    PyObject *v;

    addr += l->offset;
    switch (l->type) {
    case T_INT:
        v = PyLong_FromLong(*(int*)addr);
        break;
    case T_OBJECT:
        v = *(PyObject **)addr;
        if (v == NULL)
            v = Py_None;
       /* Py_INCREF(v); */
        break;
    default:
        PyErr_SetString(PyExc_SystemError, "bad memberdescr type");
        v = NULL;
    }
//    _mopsa_print();
    return v;
}

// FIXME: returns values with wrap_descr_set
int
PyMember_SetOne(char *addr, PyMemberDef *l, PyObject *v)
{
    PyObject *oldv;
    addr += l->offset;

    if ((l->flags & READONLY))
    {
        PyErr_SetString(PyExc_AttributeError, "readonly attribute");
        return -1;
    }
    if (v == NULL) {
        if (l->type == T_OBJECT_EX) {
            /* Check if the attribute is set. */
            if (*(PyObject **)addr == NULL) {
                PyErr_SetString(PyExc_AttributeError, l->name);
                return -1;
            }
        }
        else if (l->type != T_OBJECT) {
            PyErr_SetString(PyExc_TypeError,
                            "can't delete numeric/char attribute");
            return -1;
        }
    }
    switch (l->type) {
    case T_INT:{
        long long_val = PyLong_AsLong(v);
        if ((long_val == -1) && PyErr_Occurred())
            return -1;
        *(int *)addr = (int)long_val;
        if ((long_val > INT_MAX) || (long_val < INT_MIN))
            WARN("Truncation of value to int");
        break;
        }
    case T_OBJECT:
    case T_OBJECT_EX:
        Py_XINCREF(v);
        oldv = *(PyObject **)addr;
        *(PyObject **)addr = v;
        Py_XDECREF(oldv);
        break;
    default:
        PyErr_Format(PyExc_SystemError,
                     "bad memberdescr type for %s", l->name);
        return -1;
    }
    return 0;
}
// end of stubs

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
    printf("%s", PyModule_GetName(self));
    return (PyObject*) Py_TYPE(&CboxType);
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
