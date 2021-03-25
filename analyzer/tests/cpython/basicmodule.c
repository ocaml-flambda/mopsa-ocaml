#include <Python.h>
#include <structmember.h>

#ifdef MOPSA
#include "python_stubs.c"
#endif

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
    Py_INCREF(res); // w/o this, there will be a runtime error
    return res;
}

static PyObject*
Cbox_getcounter(Cbox *self, PyObject *args)
{
    PyObject* res = Py_BuildValue("i", self->counter); // self->contents
    Py_INCREF(res); // w/o this, there will be a runtime error
    return res;
}

static PyObject*
Cbox_getdata(Cbox *self, PyObject *args)
{
    return Py_BuildValue("Oi", self->contents, self->counter);
}

static PyObject*
Cbox_brokengetdata(Cbox *self, PyObject *args)
{
    // FIXME: doesn't seem to break in CPython :O
    return Py_BuildValue("ii", self->contents, self->counter);
}

static PyObject*
Cbox_getdatadict(Cbox *self, PyObject *args)
{
    return Py_BuildValue("{s:O,s:i}",
                         "contents", self->contents,
                         "counter", self->counter);
}

static PyObject*
Cbox_callback(Cbox *self, PyObject *args)
{
    PyObject* callback = PyTuple_GetItem(args, 0);
    if(!PyCallable_Check(callback)) {
        PyErr_SetString(PyExc_ValueError, "Argument must be a callable");
        return NULL;
    }
    PyObject* res =  PyObject_CallFunction(callback, "Oi", self->contents, self->counter);
    // probably need an incref here too
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
    if(rand()) {
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
    {"getdata", (PyCFunction) Cbox_getdata, METH_VARARGS, ""},
    {"brokengetdata", (PyCFunction) Cbox_brokengetdata, METH_VARARGS, ""},
    {"getdatadict", (PyCFunction) Cbox_getdatadict, METH_VARARGS, ""},
    {"incr", (PyCFunction) Cbox_incr, METH_VARARGS, ""},
    {"maybe_incr", (PyCFunction) Cbox_maybe_incr, METH_VARARGS, ""},
    {"maybe_incr2", (PyCFunction) Cbox_maybe_incr2, METH_VARARGS, ""},
    {"callback", (PyCFunction) Cbox_callback, METH_VARARGS, ""},
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

static PyTypeObject CounterType;

static PyObject*
Counter_richcompare(PyObject* self, PyObject* other, int op)
{
    // FIXME: implictly, we know that self has the good type, but we
    // need to explicitly check it for other. What happens if we
    // forget the check? *)
    if(!PyObject_TypeCheck(other, &CounterType))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    int cs = ((Counter*) self)->counter;
    int co = ((Counter*) other)->counter;

    int r = 0;
    switch(op)
    {
        case Py_EQ: r = cs == co; break;
        case Py_NE: r = cs != co; break;
        case Py_LE: r = cs <= co; break;
        case Py_LT: r = cs <  co; break;
        case Py_GE: r = cs >= co; break;
        case Py_GT: r = cs >  co; break;
    }
    return PyBool_FromLong(r);
}

static Py_ssize_t
Counter_len(PyObject* self)
{
    Py_ssize_t ret = ((Counter*) self)->counter;
    // FIXME: what happens if counter_len returns a negative result?
    if(ret == -1)
        PyErr_SetString(PyExc_TypeError, "uninitiliazed");
    return ret;
}

static PyObject*
Counter_item(Counter* a, Py_ssize_t i)
{
    Py_ssize_t ub = a->counter;
    if(i < ub)
        return PyLong_FromSsize_t(i);

    PyErr_SetNone(PyExc_IndexError);
    return NULL;
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


typedef struct {
    PyObject_HEAD
    int pos;
    int upper_bound;
} CounterIterator;


static PyObject*
CounterIterator_iter(PyObject* self) {
    Py_INCREF(self);
    return self;
}


static PyObject*
CounterIterator_iternext(PyObject* self) {
    CounterIterator* s = (CounterIterator*) self;
    if (s->pos < s->upper_bound)
        return PyLong_FromLong(s->pos++);
    return NULL;
}

static PyTypeObject CounterIteratorType = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "basic.CounterIterator",
    .tp_doc = "bla",
    .tp_basicsize = sizeof(CounterIterator),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_iter = CounterIterator_iter,
    .tp_iternext = CounterIterator_iternext,
};

static PyObject*
Counter_iterate(PyObject* self) {
    CounterIterator *it;
    it = PyObject_New(CounterIterator, &CounterIteratorType);
    if(it == NULL) return NULL;
    it->pos = 0;
    it->upper_bound = ((Counter*) self)->counter;
    return (PyObject *) it;
}

static PySequenceMethods counter_as_sequence = {
    (lenfunc)Counter_len,                 /* sq_length */
    0,                                  /* sq_concat */
    0,                                  /* sq_repeat */
    (ssizeargfunc)Counter_item,         /* sq_item */
    0,                                  /* sq_slice */
    0,                                  /* sq_ass_item */
    0,                                  /* sq_ass_slice */
    Counter_contains,                   /* sq_contains */
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
    .tp_iter = Counter_iterate,
    .tp_as_sequence = &counter_as_sequence,
    .tp_richcompare = Counter_richcompare
};

static PyObject*
basic_return_true(PyObject *self)
{
    Py_RETURN_TRUE;
}

static PyObject*
basic_return_false(PyObject *self)
{
    Py_RETURN_FALSE;
}

static PyObject*
basic_return_bool(PyObject *self)
{
    if(rand()) { Py_RETURN_TRUE; }
    else { Py_RETURN_FALSE; }
}

static PyObject*
basic_return_none(PyObject *self)
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
basic_subtype(PyObject *self, PyObject *args)
{
    PyObject* a;
    PyObject* b;
    if(!PyArg_ParseTuple(args, "OO", &a, &b))
        return NULL;

    if(!PyType_Check(a) || !PyType_Check(b)) {
        PyErr_SetString(PyExc_TypeError, "basic_subtype expects classes");
        return NULL;
    }

    return PyBool_FromLong(PyType_IsSubtype((PyTypeObject*)a, (PyTypeObject*)b));
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
basic_id_check_methzero(PyObject *self, PyObject *args)
{
    return args;
}

static PyObject*
basic_random_fail(PyObject *self, PyObject *args)
{
    PyObject* r;
    if(rand()) { r = basic_id_check(self, args); }
    else { r = basic_raise_exc(self, args); }
    return r;
}


static PyObject*
basic_vsum(PyObject *self, PyObject *args)
{
    int a, b, c, d;
    b = 0;
    c = 0;
    d = 0;
    if(!PyArg_ParseTuple(args, "i|iii", &a, &b, &c, &d))
        return NULL;

    return Py_BuildValue("i", a+b+c+d);
}

static PyObject*
basic_vsum2(PyObject* self, PyObject *args)
{
    PyObject *a = NULL;
    PyObject *b = NULL;
    PyObject *c = NULL;
    if(!PyArg_UnpackTuple(args, "", 1, 3, &a, &b, &c))
        return NULL;

    int va = 0, vb = 0, vc = 0;
    if(a) va = PyLong_AsLong(a);
    if(b) vb = PyLong_AsLong(b);
    if(c) vc = PyLong_AsLong(c);

    return Py_BuildValue("i", va+vb+vc);
}

static PyObject*
basic_weirdtuple(PyObject* self, PyObject* args)
{
    PyObject* a = PyTuple_New(10);
    PyTuple_SetItem(a, 0, PyLong_FromLong(0));
    PyTuple_SetItem(a, 9, PyLong_FromLong(9));
    return a;
}

static PyObject*
basic_twistedcbox(PyObject* self, PyObject* args)
{
    return PyObject_CallObject((PyObject*)&CboxType, args);
}

static PyObject*
basic_sequence_tail(PyObject* self, PyObject* args)
{
    PyObject* s = PyTuple_GetItem(args, 0);

    int b = PySequence_Check(s);
    if(!b) {
        PyErr_SetString(PyExc_TypeError, "sequence_tail requires argument supporting sequence protocol");
    return NULL;
    }

    int ls = PySequence_Size(s);
    if(ls <= 0) {
        PyErr_SetString(PyExc_ValueError, "sequence_tail requires non-empty sequence");
        return NULL;
    }

    return PySequence_GetItem(s, -1);
}

static PyObject*
basic_compare(PyObject* self, PyObject* args)
{
    PyObject* l;
    PyObject* r;
    int flag;
    if(!PyArg_ParseTuple(args, "OOi", &l, &r, &flag))
        return NULL;

    if(!(Py_LT <= flag && flag <= Py_GE)) {
        PyErr_SetString(PyExc_ValueError, "3rd argument must be in [0, 5]");
        return NULL;
    }

    int rb = PyObject_RichCompareBool(l, r, flag);
    PyObject* ro = PyObject_RichCompare(l, r, flag);

    if(!(rb ? (ro == Py_True) : (ro == Py_False))) {
        PyErr_SetString(PyExc_SystemError, "assertion failed");
        return NULL;
    }

    return ro;
}



static PyObject*
basic_kwds(PyObject* self, PyObject* args, PyObject *kwds)
{
    static char *kwlist[] = {"a", "b", "c", NULL};
    int a = 1, b = 1, c = 1;
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "|iii", kwlist, &a, &b, &c))
        return NULL;

    return Py_BuildValue("i", a*b*c);
}


static PyMethodDef module_methods[] = {
    {"typ", (PyCFunction) basic_typ, METH_VARARGS, ""},
    {"subtype", (PyCFunction) basic_subtype, METH_VARARGS, ""},
    {"raise_exc", (PyCFunction) basic_raise_exc, METH_VARARGS, ""},
    {"forget_raise", (PyCFunction) basic_forget_raise, METH_VARARGS, ""},
    {"id_check", (PyCFunction) basic_id_check, METH_VARARGS, ""},
    {"id_check_methzero", (PyCFunction) basic_id_check_methzero, METH_O, ""},
    {"random_fail", (PyCFunction) basic_random_fail, METH_VARARGS, ""},
    {"return_true", (PyCFunction) basic_return_true, METH_NOARGS, ""},
    {"return_false", (PyCFunction) basic_return_false, METH_NOARGS, ""},
    {"return_bool", (PyCFunction) basic_return_bool, METH_NOARGS, ""},
    {"return_none", (PyCFunction) basic_return_none, METH_NOARGS, ""},
    {"vsum", (PyCFunction) basic_vsum, METH_VARARGS, ""},
    {"vsum2", (PyCFunction) basic_vsum2, METH_VARARGS, ""},
    {"weirdtuple", (PyCFunction) basic_weirdtuple, METH_VARARGS, ""},
    {"twistedcbox", (PyCFunction) basic_twistedcbox, METH_VARARGS, ""},
    {"sequence_tail", (PyCFunction) basic_sequence_tail, METH_VARARGS, ""},
    {"compare", (PyCFunction) basic_compare, METH_VARARGS, ""},
    {"kwds", (PyCFunction) basic_kwds, METH_VARARGS | METH_KEYWORDS, ""},
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
    if (PyType_Ready(&CounterIteratorType)) return NULL;
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
    PyModule_AddObject(m, "version_str", Py_BuildValue("s", "0.0.0"));
    return m;
}
