// stubs used by the analysis
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
PyErr_Occurred()
{
    return exc_state;
};

void
PyErr_Clear()
{
    exc_state = NULL;
    exc_msg = NULL;
}

void PyErr_SetNone(PyObject* exc) {
    exc_state = exc;
}

void PyErr_SetString(PyObject* exc, const char* msg){
    exc_state = exc;
    exc_msg = msg;
//    _mopsa_print();
}

void PyLong_AsLong_Helper(){
    PyErr_SetString(PyExc_OverflowError,
                    "Python int too large to convert to C long");
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

    /* somehow PyObject_INIT? */
    // Py_TYPE(obj) = type; // now done by the boundary. Maybe the refcnt should be too?
    obj->ob_refcnt = 1;

    /* FIXME */
    if (obj == NULL)
        return PyErr_NoMemory();

    return obj;
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

    // Py_TYPE(obj) = type;
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
        _mopsa_print();
        return 0;
    }
    Py_DECREF(o);
    return -1;
}


static PyObject *
wrap_init(PyObject *self, PyObject *args, void *wrapped, PyObject *kwds)
{
    initproc func = (initproc)wrapped;

    if (func(self, args, kwds) < 0)
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
    _mopsa_print();
}

void
init_flags()
{
    PyType_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_TYPE_SUBCLASS;
    PyBaseObject_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
    PyLong_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_LONG_SUBCLASS;
    PyUnicode_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_UNICODE_SUBCLASS;
    PyList_Type.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_LIST_SUBCLASS;
}

void set_default_flags(PyTypeObject *t)
{
    // should be 284160
    // 9, 10, 12, 14, 18
    t->tp_flags = Py_TPFLAGS_HEAPTYPE | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_READY | Py_TPFLAGS_HAVE_GC | Py_TPFLAGS_HAVE_VERSION_TAG;
}
