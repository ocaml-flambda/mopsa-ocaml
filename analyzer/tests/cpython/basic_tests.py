import unittest
import basic

# try using PyObject_New

class A:
    def __init__(self, x):
        self.x = x

class Test(unittest.TestCase):
    def test_module_addobject(self):
        self.assertTrue(hasattr(basic, "Cbox"))

    def test_module_addintconstant(self):
        self.assertEqual(basic.version, 0)

    def test_module_addstringconstant(self):
        self.assertEqual(basic.version_str, "0.0.0")

    def test_type_int(self):
        # tests c function calls, PyArg_ParseTuple on one object, and Py_TYPE
        self.assertEqual(basic.typ(0), int)

    def test_type_pyclass(self):
        self.assertEqual(basic.typ(A(1)), A)

    def test_call_failure(self):
        with self.assertRaises(TypeError):
            self.assertEqual(basic.typ(A()), A)

    def test_type_cclass(self):
        self.assertEqual(basic.typ(basic.Cbox(1, 0)), basic.Cbox)
        self.assertEqual(basic.typ(basic.Cbox), type)
        # FIXME
        # self.assertEqual(basic.typ(TypeError), type)
        # self.assertEqual(basic.typ(TypeError()), TypeError)

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
        r = c.getcontents()
        self.assertEqual(r, a)
        self.assertEqual(c.contents, a)
        a.x = 3
        self.assertEqual(c.contents.x, a.x)
        c.contents.x = 4
        self.assertEqual(a.x, c.contents.x)
        self.assertEqual(c.counter, 3)
        self.assertEqual(c.incr(), None)
        self.assertEqual(c.counter, 4)
        c.counter += 3
        self.assertEqual(c.counter, 7)

    def test_c_subclass(self):
        class C(basic.Cbox): pass
        a = A(1)
        c = C(a, 3)
        self.assertIsInstance(c, basic.Cbox)
        self.assertIsInstance(c, C)
        self.assertEqual(c.getcontents(), a)
        self.assertEqual(c.contents, a)
        a.x = 3
        self.assertEqual(c.contents.x, a.x)
        c.contents.x = 4
        self.assertEqual(a.x, c.contents.x)
        self.assertEqual(c.counter, 3)
        self.assertEqual(c.incr(), None)
        self.assertEqual(c.counter, 4)
        c.counter += 3
        r = c.counter
        self.assertEqual(r, 7)

    def test_c_casesplit(self):
        a = A(1)
        c = basic.Cbox(a, 3)
        try:
            c.maybe_incr()
            d = c.counter
            self.assertEqual(d, 4)
        except SystemError:
            pass

    def test_c_casesplit2(self):
        a = A(1)
        c = basic.Cbox(a, 3)
        try:
            c.maybe_incr2()
            d = c.counter
            self.assertTrue(3 <= d and d <= 4)
        except SystemError:
            pass

    def test_c_overflow(self):
        # overflowerror from PyLong_AsLong
        with self.assertRaises(OverflowError):
            c = basic.Cbox([], 9223372036854775808)
        with self.assertRaises(OverflowError):
            c = basic.Cbox([], 9223372036854775807)

    def test_buildvalue(self):
        class A: pass
        a = A()
        c = basic.Cbox(a, 3)
        c.incr()
        d = c.getcounter()
        self.assertEqual(d, 4)

    def test_buildvalue2(self):
        class A: pass
        a = A()
        c = basic.Cbox(a, 4)
        tup = c.getdata()
        self.assertEqual(len(tup), 2)
        self.assertEqual(tup[0], a)
        self.assertEqual(tup[1], 4)

    def test_buildvalue3(self):
        class A: pass
        a = A()
        c = basic.Cbox(a, 4)
        tup = c.getdatadict()
        self.assertEqual(tup['contents'], a)
        self.assertEqual(tup['counter'], 4) # always false due to disjunctive evaluation
        # FIXME: brokengetdata to try

    def test_member_type_restriction(self):
        c = basic.Cbox(1, 3)
        with self.assertRaises(TypeError):
            c.counter = 'abcd'

    def test_member_readonly_flag(self):
        c = basic.Cbox(1, 3)
        with self.assertRaises(AttributeError):
            c.contents = 2

    def test_unicode_check_length(self):
        c = basic.Cbox('abcdef', 3)
        self.assertEqual(c.counter, 6)

    def test_id_check(self):
        with self.assertRaises(TypeError):
            basic.id_check()
        with self.assertRaises(TypeError):
            basic.id_check(1,2)
        self.assertEqual(basic.id_check(1), 1)
        self.assertEqual(basic.id_check('abc'), 'abc')
        a = A(3)
        self.assertEqual(basic.id_check(a), a)

    def test_meth_zero(self):
        # FIXME: need to perform the check
        # with self.assertRaises(TypeError):
        #     basic.id_check_methzero()
        # with self.assertRaises(TypeError):
        #     basic.id_check_methzero(1,2)
        self.assertEqual(basic.id_check_methzero(1), 1)
        self.assertEqual(basic.id_check_methzero('abc'), 'abc')
        a = A(3)
        self.assertEqual(basic.id_check_methzero(a), a)

    # counter tests: PyType_GenericNew, sq_len+wrapper, PyLong_Check, PyLong_AsSsize_t
    def test_counter(self):
        with self.assertRaises(TypeError):
            basic.Counter('abcd')
        c1 = basic.Counter(3)
        self.assertEqual(c1.__len__(), 3)
        c2 = basic.Counter(-2)
        self.assertEqual(c2.__len__(), -2) # FIXME
        c3 = basic.Counter(-1)
        with self.assertRaises(TypeError):
            c3.__len__()
        self.assertTrue((-1) in c3)
        self.assertFalse(0 in c3)

    def test_bools(self):
        self.assertTrue(basic.return_true())
        self.assertTrue(not basic.return_false())
        self.assertTrue(basic.return_true() and not basic.return_false())

    def test_none(self):
        self.assertEqual(basic.return_none(), None)


    def test_new_wrapper(self):
        with self.assertRaises(SystemError):
            c = basic.Cbox('a', -1)

    def test_iteration(self):
        c = basic.Counter(4)
        it = iter(c)
        self.assertEqual(next(it), 0)
        self.assertEqual(next(it), 1)
        self.assertEqual(next(it), 2)
        self.assertEqual(next(it), 3)
        with self.assertRaises(StopIteration):
            r = next(it)

    def test_callback(self):
        a = A(1)
        z0 = a.x
        c = basic.Cbox(a, 3)
        a0 = c.getcontents()
        z1 = a0.x
        def callback1(contents, counter):
            r = contents.x
            return r + counter
        def callback2(contents, counter):
            contents.x = counter
        def callback3(contents, counter):
            return (contents, counter)
        def callback4(contents, counter):
            return contents + counter # will raise a TypeError
        r1 = c.callback(callback1)
        self.assertEqual(r1, 4)
        r2 = c.callback(callback2)
        self.assertEqual(r2, None)
        self.assertEqual(c.contents.x, 3)
        self.assertEqual(a.x, 3)
        r3 = c.callback(callback3)
        self.assertEqual(r3[0], a)
        self.assertEqual(r3[1], 3)
        with self.assertRaisesRegex(TypeError, "unsupported operand type(s) for '+': .*"):
            r4 = c.callback(callback4)
        # with self.assertRaisesRegex(TypeError, "Argument must be a callable"):
        #     r5 = c.callback("notafunction")

    def test_callback_object(self):
        class A: pass
        a = A()
        c = basic.twistedcbox(a, 3)
        self.assertTrue(isinstance(c, basic.Cbox))
        self.assertEqual(c.contents, a)
        self.assertEqual(c.counter, 3)

    def test_variable_args(self):
        with self.assertRaisesRegex(TypeError, "function takes at least 1 argument (0 given)"):
            basic.vsum()
        with self.assertRaisesRegex(TypeError, "function takes at most 4 arguments (5 given)"):
            basic.vsum(1,2,3,4,5)
        self.assertEqual(basic.vsum(1), 1)
        self.assertEqual(basic.vsum(1,2), 3)
        self.assertEqual(basic.vsum(1,2,3), 6)
        self.assertEqual(basic.vsum(1,2,3,4), 10)

    def test_variable_args2(self):
        with self.assertRaisesRegex(TypeError, "function takes at least 1 argument (0 given)"):
            basic.vsum2()
        with self.assertRaisesRegex(TypeError, "function takes at most 3 arguments (5 given)"):
            basic.vsum2(1,2,3,4,5)
        r1 = basic.vsum2(1)
        self.assertEqual(r1, 1)
        r2 = basic.vsum2(1, 2)
        self.assertEqual(r2, 3)
        r3 = basic.vsum2(1, 2, 3)
        self.assertEqual(r3, 6)

    def test_issubtype(self):
        self.assertFalse(basic.subtype(basic.Cbox, basic.Counter))
        self.assertTrue(basic.subtype(basic.Cbox, basic.Cbox))
        self.assertTrue(basic.subtype(basic.Cbox, object))
        self.assertTrue(basic.subtype(list, object))
        self.assertTrue(basic.subtype(object, object))
        self.assertTrue(basic.subtype(type(basic.Cbox([], 3)), basic.Cbox))
        with self.assertRaisesRegex(TypeError, "basic_subtype expects classes"):
            basic.subtype(1, [])
            #FIXME: what happens if we forget the PyType_Check casts?
            #       issue happens in Python side. Maybe we should check the types in the boundary?

    def test_weirdtuple(self):
        t = basic.weirdtuple()
        self.assertEqual(t[0], 0)
        self.assertEqual(t[9], 9)
        # FIXME: this is a segfault, we should handle it better
        with self.assertRaises(NameError):
            r = t[3]

    def test_sequence(self):
        # FIXME: now define B with __getitem__ and __length__ and check that it works...
        class A: pass
        with self.assertRaises(TypeError):
            basic.sequence_tail(1)
        with self.assertRaises(TypeError):
            basic.sequence_tail({1:2})
        with self.assertRaises(TypeError):
            basic.sequence_tail(A())
        with self.assertRaises(TypeError):
            basic.sequence_tail(A)

        with self.assertRaises(ValueError):
            basic.sequence_tail([])
        with self.assertRaises(ValueError):
            basic.sequence_tail(())
        with self.assertRaises(ValueError):
            basic.sequence_tail(basic.Counter(0))

        self.assertEqual(basic.sequence_tail([1,2,3]), 3)
        self.assertEqual(basic.sequence_tail((1,2,3)), 3)
        self.assertEqual(basic.sequence_tail(basic.Counter(10)), 9)


    def test_richcompare(self):
        c1 = basic.Counter(1)
        c2 = basic.Counter(2)
        self.assertTrue(c1 < c2)
        self.assertTrue(c1 <= c2)
        self.assertFalse(c1 == c2)
        self.assertTrue(c1 != c2)
        self.assertFalse(c1 > c2)
        self.assertFalse(c1 >= c2)

        self.assertFalse(c1 < c1)
        self.assertTrue(c1 <= c1)
        self.assertTrue(c1 == c1)
        self.assertFalse(c1 != c1)
        self.assertFalse(c1 > c1)
        self.assertTrue(c1 >= c1)

        with self.assertRaises(TypeError):
            c1 < 1

    def test_object_compare(self):
        c1 = 1
        c2 = 2
        self.assertTrue(basic.compare(c1, c2, 0))
        self.assertTrue(basic.compare(c1, c2, 1))
        self.assertFalse(basic.compare(c1, c2, 2))
        self.assertTrue(basic.compare(c1, c2, 3))
        self.assertFalse(basic.compare(c1, c2, 4))
        self.assertFalse(basic.compare(c1, c2, 5))
        with self.assertRaises(ValueError):
            basic.compare(1, 2, 7)

        self.assertFalse(basic.compare(c1, c1, 0))
        self.assertTrue(basic.compare(c1, c1, 1))
        self.assertTrue(basic.compare(c1, c1, 2))
        self.assertFalse(basic.compare(c1, c1, 3))
        self.assertFalse(basic.compare(c1, c1, 4))
        self.assertTrue(basic.compare(c1, c1, 5))

    def test_kwds_and_tuple(self):
        # FIXME: do this we chars and concatenate, or build a tuple, to be sure arguments are not swapped
        kwds = basic.kwds
        self.assertEqual(kwds(), (1000, (1, 10, 100)))
        self.assertEqual(kwds(3), (3000, (3, 10, 100)))
        self.assertEqual(kwds(3, b = 2), (600, (3, 2, 100)))
        self.assertEqual(kwds(2, 3, 5), (30, (2, 3, 5)))
        with self.assertRaises(TypeError):
            kwds(3, a = 2)
        with self.assertRaises(TypeError):
            kwds(2, 3, 5, 7)

    def test_pb_float(self):
        f = basic.pb_float(3.141592653589793115997963468544185161590576171875)
        self.assertEqual(f, 4.14159297943115234375)
        with self.assertRaises(TypeError):
            basic.pb_float("abc")

    def test_checklen(self):
        l = "smith"
        cl = basic.checklen(l)
        pyl = len(l)
        self.assertEqual(cl, pyl)

if __name__ == "__main__":
    unittest.main()
