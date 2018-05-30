import mopsa

class TestCase:
    @mopsa.builtin("unittest.TestCase.assertTrue")
    def assertTrue(self, cond): pass

    @mopsa.builtin("unittest.TestCase.assertFalse")
    def assertFalse(self, cond): pass

    @mopsa.builtin("unittest.TestCase.assertEqual")
    def assertEqual(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIs")
    def assertIs(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIsNot")
    def assertIsNot(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIn")
    def assertIn(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertNotIn")
    def assertNotIn(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertIsInstance")
    def assertIsInstance(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertNotIsInstance")
    def assertNotIsInstance(self, x, y): pass

    @mopsa.builtin("unittest.TestCase.assertRaises")
    def assertRaises(self, exn, f, args): pass

    @mopsa.builtin("unittest.TestCase.fail")
    def fail(self, msg): pass

class ExceptionContext(object):
    def __init__(self, exn):
        self.expected = exn

    def __enter__(self):
        return self

    @mopsa.builtin("unittest.TestCase.assertTrue")
    def __exit__(self, type, exn, trace): pass

@mopsa.builtin("unittest.main")
def main(): pass
