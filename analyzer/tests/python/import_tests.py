import unittest

class A(unittest.TestCase):
    def test1(self):
        import import_test
        self.assertEqual(import_test.y, "a")
        self.assertEqual(type(import_test.othersubdir), type(unittest))
        r0 = import_test.othersubdir.z
        self.assertEqual(len(r0), 0)
        self.assertEqual(type(r0), list)
        self.assertEqual(import_test.xsubdir, 2)
        self.assertEqual(import_test.subdir.x, 2)
        self.assertEqual(import_test.bluuu, 42)
        self.assertEqual(type(import_test.yetanother), type(unittest))
        self.assertEqual(import_test.yetanother.bla, "bli")
        self.assertEqual(import_test.imported_bla, import_test.yetanother.bla)

    def test2(self):
        from import_test import othersubdir
        r0 = othersubdir.z
        self.assertEqual(len(r0), 0)
        self.assertEqual(type(r0), list)
        self.assertEqual(type(othersubdir), type(unittest))

    def test3(self):
    # FIXME: without the from import_test.subdir import: we should try import import_test.subdir as subdir or something...
        from import_test import subdir
        self.assertEqual(type(subdir), type(unittest))
        self.assertEqual(subdir.x, 2)

if __name__ == "__main__":
    unittest.main()
