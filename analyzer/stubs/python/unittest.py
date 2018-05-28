import mopsa

class TestCase:
    def assertTrue(self, cond): pass
    def assertFalse(self, cond): pass
    def assertEqual(self, x, y): pass
    def assertIs(self, x, y): pass
    def assertIsNot(self, x, y): pass
    def assertIn(self, x, y): pass
    def assertNotIn(self, x, y): pass
    def assertIsInstance(self, x, y): pass
    def assertNotIsInstance(self, x, y): pass
    def assertRaises(self, exn, f, args): pass
    def fail(self, msg): pass

class ExceptionContext(object):
    @mopsa.stub
    def __init__(self, exn):
        self.expected = exn

    @mopsa.stub
    def __enter__(self):
        return self

    def __exit__(self, type, exn, trace): pass

def main(): pass
