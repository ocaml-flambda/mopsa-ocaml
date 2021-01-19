import mopsa
import unittest
import basic

class A:
    def __init__(self, x):
        self.x = x

class Test(unittest.TestCase):
    def test_module_addobject(self):
        self.assertTrue(hasattr(basic, "Cbox"))

    def test_module_addintconstant(self):
        self.assertEqual(basic.version, 0)

    def test_type_int(self):
        # tests c function calls, PyArg_ParseTuple on one object, and Py_TYPE
        self.assertEqual(basic.typ(0), int)

    def test_type_pyclass(self):
        self.assertEqual(basic.typ(A(1)), A)

    # FIXME
    # def test_call_failure(self):
    #     self.assertEqual(basic.typ(A()), A)

    def test_type_cclass(self):
        self.assertEqual(basic.typ(basic.Cbox(1, 0)), basic.Cbox)

    def test_c_exn(self):
        with self.assertRaises(AttributeError):
            basic.raise_exc()

    def test_c_null_no_exn(self):
        with self.assertRaises(SystemError):
            basic.forget_raise()

    def test_c_class(self):
        a = A(1)
        c = basic.Cbox(a, 3)
        self.assertIsInstance(c, basic.Cbox)
        self.assertEqual(c.getcontents(), a)
        self.assertEqual(c.contents, a)
        # self.assertEqual(c.counter, 3)
        # c.incr()
        # self.assertEqual(c.counter, 4)
        # c.counter += 3
        # self.assertEqual(c.counter, 7)

    # def test_member_type_restriction(self):
    #     c = basic.Cbox(1, 3)
    #     c.counter = 'abcd'# fixme

    def test_member_readonly_flag(self):
        c = basic.Cbox(1, 3)
        with self.assertRaises(AttributeError):
            c.contents = 2


    def test_unicode_check_length(self):
        c = basic.Cbox('abcdef', 3)
        self.assertEqual(c.counter, 6)

if __name__ == "__main__":
    unittest.main()



# essayer une classe avec un alloc sutom et l'autre avec PyObject_New

# int/str conversion after function <- through "fst" function returning fst argument?

# tp_len + check wrapper (d'autres wrapper?)
# Long_Check

# PyTuple_Size, PyTuple_GetItem
