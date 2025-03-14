#include <Python.h>
#include "structmember.h"

#ifdef MOPSA
#include "python_stubs.c"
#endif

typedef struct {
  PyObject ob_base;
  int count;
} CounterObject;

static PyObject*
CounterIncr(CounterObject *self, PyObject *args)
{
  int i = 1;
  if(!PyArg_ParseTuple(args, "|i", &i))
    return NULL;
  self->count += i;
  Py_RETURN_NONE;
}

static int
CounterInit(CounterObject *self, PyObject *args,
            PyObject *kwds)
{
  self->count = 0;
  return 0;
}

static PyMethodDef CounterMethods[] = {
 {"incr", (PyCFunction) CounterIncr,
  METH_VARARGS, ""}, {NULL}
};

static PyMemberDef CounterMembers[] = {
 {"counter", T_INT, offsetof(CounterObject, count),
  READONLY, ""}, {NULL}
};

static PyType_Slot CounterTypeSlots[] = {
  {Py_tp_new, PyType_GenericNew},
  {Py_tp_init, CounterInit},
  {Py_tp_methods, CounterMethods},
  {Py_tp_members, CounterMembers}, {0, 0}
};

static PyType_Spec CounterTypeSpec = {
  .name = "counter.Counter",
  .basicsize = sizeof(CounterObject),
  .itemsize = 0,
  .flags = Py_TPFLAGS_DEFAULT
         | Py_TPFLAGS_BASETYPE,
  .slots = CounterTypeSlots
};

static struct PyModuleDef countermod = {
 PyModuleDef_HEAD_INIT, .m_name = "counter",
  .m_methods = NULL, .m_size = -1
};

PyMODINIT_FUNC
PyInit_counter(void)
{
  PyObject *m = PyModule_Create(&countermod);
  if(m == NULL) return NULL;
  PyObject* CounterType = PyType_FromSpec(&CounterTypeSpec);
  if(CounterType == NULL || PyModule_AddObject(m, "Counter", CounterType) < 0) {
    Py_DECREF(m);
    return NULL;
  }
  return m;
}
