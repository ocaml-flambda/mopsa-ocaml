#undef PyArg_ParseTuple
#undef PyArg_ParseTupleAndKeywords
#undef Py_BuildValue


/* // stubs used by the analysis */
typedef struct exc_data {
    PyObject* exc_state;
    char* exc_msg;
} exc_data;

int _mopsa_pyerr_bind_cs_to(exc_data*);

exc_data *exc;

PyObject*
PyErr_NoMemory()
{
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = PyExc_MemoryError;
    exc->exc_msg = NULL;
    _mopsa_pyerr_bind_cs_to(exc);
    return NULL;
}

PyObject*
PyErr_Occurred()
{
    return exc->exc_state;
}

char* PyErr_GetMsg() // not a real cpython function
{
    return exc->exc_msg;
}

void
PyErr_Clear()
{
    free(exc);
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = NULL;
    exc->exc_msg = NULL;
}

void PyErr_SetNone(PyObject* o) {
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = o;
    exc->exc_msg = NULL;
    _mopsa_pyerr_bind_cs_to(exc);
}

void PyErr_SetString(PyObject* o, const char* msg){
    exc = malloc(sizeof(exc_data));
    _mopsa_assume(exc != NULL);
    exc->exc_state = o;
    exc->exc_msg = msg;
    _mopsa_pyerr_bind_cs_to(exc);
}

PyObject*
PyErr_Format(PyObject* o, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    char* msg;
    vasprintf(&msg, fmt, args);
    va_end(args);
    PyErr_SetString(o, msg);
    return NULL;
}

int PyErr_BadArgument(void)
{
    PyErr_SetString(PyExc_TypeError,
                    "bad argument type for built-in operation");
    return 0;
}


int PyType_ReadyCheat(PyTypeObject *type)
{
    Py_TYPE(type) = &PyType_Type;
    type->tp_flags =
        (type->tp_flags & ~Py_TPFLAGS_READYING) | Py_TPFLAGS_READY;
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

    /* somehow PyObject_INIT? */
    Py_TYPE(obj) = type; // FIXME now done by the boundary. Maybe the refcnt should be too?
    obj->ob_refcnt = 1;

    /* FIXME */
    if (obj == NULL)
        return PyErr_NoMemory();

    return obj;
}

PyObject *
PyType_GenericNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    return type->tp_alloc(type, 0);
}


PyObject*
_PyObject_New(PyTypeObject *type)
{
    PyObject *obj;
    const size_t size = _PyObject_SIZE(type);
    obj = PyType_GenericAlloc_Helper(type, size);
    memset(obj, '\0', size);

    if (obj == NULL)
        return PyErr_NoMemory();

    Py_TYPE(obj) = type;
    obj->ob_refcnt = 1;

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

int PyModule_AddIntConstant(PyObject *m, const char *name, long value)
{
    PyObject *o = PyLong_FromLong(value);
    if(!o)
        return -1;
    if (PyModule_AddObject(m, name, o) == 0)
    {
        return 0;
    }
    Py_DECREF(o);
    return -1;
}


static PyObject *
wrap_init(PyObject *self, PyObject *args, void *wrapped, PyObject *kwds)
{
    initproc func = (initproc)wrapped;
    int r = func(self, args, kwds);
    if (r < 0)
        return NULL;
    Py_RETURN_NONE;
}

static PyObject *
wrap_lenfunc(PyObject *self, PyObject *args, void *wrapped)
{
    lenfunc func = (lenfunc)wrapped;
    Py_ssize_t res;

    /* if (!check_num_args(args, 0)) */
    /*     return NULL; */
    res = (*func)(self);
    if (res == -1 && PyErr_Occurred())
        return NULL;
    return PyLong_FromSsize_t(res);
}

static PyObject *
wrap_objobjproc(PyObject *self, PyObject *args, void *wrapped)
{
    objobjproc func = (objobjproc)wrapped;
    int res;
    PyObject *value;

    // FIXME
    /* if (!check_num_args(args, 1)) */
    /*     return NULL; */
    value = PyTuple_GetItem(args, 0);// was GET_ITEM
    res = (*func)(self, value);
    if (res == -1 && PyErr_Occurred())
        return NULL;
    else
        return PyBool_FromLong(res);
}

static PyObject *
wrap_unaryfunc(PyObject *self, PyObject *args, void *wrapped)
{
    unaryfunc func = (unaryfunc)wrapped;

    /* if (!check_num_args(args, 0)) */
    /*     return NULL; */
    return (*func)(self);
}

static PyObject *
wrap_next(PyObject *self, PyObject *args, void *wrapped)
{
    unaryfunc func = (unaryfunc)wrapped;
    PyObject *res;

    /* if (!check_num_args(args, 0)) */
    /*     return NULL; */
    res = (*func)(self);
    if (res == NULL && !PyErr_Occurred())
        PyErr_SetNone(PyExc_StopIteration);
    return res;
}


static PyObject *
tp_new_wrapper(PyObject *self, PyObject *args, PyObject *kwds)
{
    PyTypeObject *type, *subtype, *staticbase;
    PyObject *arg0, *res;
    PyTypeObject *type1;

    /* if (self == NULL || !PyType_Check(self)) */
    /*     Py_FatalError("__new__() called with non-type 'self'"); */
    type = (PyTypeObject *)self;
    /* if (!PyTuple_Check(args) || PyTuple_GET_SIZE(args) < 1) { */
    /*     PyErr_Format(PyExc_TypeError, */
    /*                  "%s.__new__(): not enough arguments", */
    /*                  type->tp_name); */
    /*     return NULL; */
    /* } */
    arg0 = PyTuple_GetItem(args, 0);
    type1 = &PyLong_Type;
    /* if (!PyType_Check(arg0)) { */
    /*     PyErr_Format(PyExc_TypeError, */
    /*                  "%s.__new__(X): X is not a type object (%s)", */
    /*                  type->tp_name, */
    /*                  Py_TYPE(arg0)->tp_name); */
    /*     return NULL; */
    /* } */
    subtype = (PyTypeObject *)arg0;
    /* if (!PyType_IsSubtype(subtype, type)) { */
    /*     PyErr_Format(PyExc_TypeError, */
    /*                  "%s.__new__(%s): %s is not a subtype of %s", */
    /*                  type->tp_name, */
    /*                  subtype->tp_name, */
    /*                  subtype->tp_name, */
    /*                  type->tp_name); */
    /*     return NULL; */
    /* } */

    args = PyTuple_GetSlice(args, 1, PyTuple_Size(args));
    /* if (args == NULL) */
    /*     return NULL; */
    res = type->tp_new(subtype, args, kwds);
    /* Py_DECREF(args); */
    return res;
}


// FIXME: incomplete, defined in abstract.c
Py_ssize_t
PyNumber_AsSsize_t(PyObject *item, PyObject *err)
{
    Py_ssize_t result;
    PyObject *runerr;
    PyObject *value = PyNumber_Index(item);
    if (value == NULL)
        return -1;

    /* We're done if PyLong_AsSsize_t() returns without error. */
    result = PyLong_AsSsize_t(value);
    return result;
}


// FIXME: incomplete, defined in abstract.c
PyObject*
PyNumber_Index(PyObject *item)
{
    PyObject *result = NULL;
    if (item == NULL) {
        return null_error();
    }

    if (PyLong_Check(item)) {
        Py_INCREF(item);
        return item;
    }//    if (!PyIndex_Check(item))
    else
    {
        PyErr_Format(PyExc_TypeError,
                     "'%.200s' object cannot be interpreted "
                     "as an integer", item->ob_type->tp_name);
        return NULL;
    }
}



void *
PyMem_Malloc(size_t size)
{
    if (size == 0)
        size = 1;
    return malloc(size);
}

void *
PyMem_Calloc(size_t nelem, size_t elsize)
{
    if (nelem == 0 || elsize == 0) {
        nelem = 1;
        elsize = 1;
    }
    return calloc(nelem, elsize);
}

void *
PyMem_Realloc(void *ptr, size_t size)
{
    if (size == 0)
        size = 1;
    return realloc(ptr, size);
}

void
PyMem_Free(void *ptr)
{
    free(ptr);
}

void
_PyType_Assign_Helper(PyObject* obj, PyTypeObject* type)
// in practice, only used to get the types PyObject and PyTypeObject...
{
    *(PyTypeObject**) ((char*)obj + offsetof(PyObject, ob_type)) = type;
}

void
init_flags()
{
    // all flags are in the object declaration except Ready, which is
    // added upon completion of PyType_Ready
    PyType_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_TYPE_SUBCLASS | Py_TPFLAGS_READY;
    PyBaseObject_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY;
    PyLong_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_LONG_SUBCLASS | Py_TPFLAGS_READY;
    PyUnicode_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_UNICODE_SUBCLASS | Py_TPFLAGS_READY;
    PyList_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_LIST_SUBCLASS | Py_TPFLAGS_READY;
    PyTuple_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_TUPLE_SUBCLASS | Py_TPFLAGS_READY;
    _PyNone_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_READY;
    PyBytes_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_BYTES_SUBCLASS | Py_TPFLAGS_READY;
    PyDict_Type.tp_flags =  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_DICT_SUBCLASS | Py_TPFLAGS_READY;
}

PyObject _Py_NoneStruct = {
  _PyObject_EXTRA_INIT
  1, &_PyNone_Type
};

void set_default_flags(PyTypeObject *t)
{
    // should be 284160
    // 9, 10, 12, 14, 18
    t->tp_flags = Py_TPFLAGS_HEAPTYPE | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_HAVE_VERSION_TAG;
}


Py_UCS4* PyUnicode_AsUCS4Copy(PyObject *unicode)
{
    // let's be imprecise: we malloc a buffer of the good size, but don't copy anything
    Py_ssize_t s = PyUnicode_GetLength(unicode);
    if(s == -1 && PyErr_Occurred())
        return NULL;
    Py_UCS4* ret = malloc(s * sizeof(Py_UCS4));
    if(ret)
    {
        return ret;
    }
    else
    {
        PyErr_NoMemory();
        return NULL;
    }
}

int PyParseTuple_int_helper(PyObject *obj, int *result)
{
    long ival = PyLong_AsLong(obj);
    if(ival == -1 && PyErr_Occurred()) {
        return 0;
    }
    else if (ival > INT_MAX) {
        PyErr_SetString(PyExc_OverflowError,
                        "signed integer is greater than maximum");
        return 0;
    }
    else if (ival < INT_MIN) {
        PyErr_SetString(PyExc_OverflowError,
                        "signed integer is less than minimum");
        return 0;
    }
    else {
        *result = ival;
        return 1;
    }
}


PyObject *PyBool_FromLong(long ok)
{
    PyObject *result;

    if (ok)
        result = Py_True;
    else
        result = Py_False;
    Py_INCREF(result);
    return result;
}
