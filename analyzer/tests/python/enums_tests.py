import unittest

class RangeTests(unittest.TestCase):
    def test_range_getitem(self):
        r = range(1, 10, 3)
        self.assertEqual(r[0], 1)
        self.assertEqual(r[1], 4)
        self.assertEqual(r[2], 7)
        self.assertEqual(r[-1], 7)
        self.assertEqual(r[-2], 4)
        self.assertEqual(r[-3], 1)
        with self.assertRaises(IndexError):
            r[3]
        with self.assertRaises(IndexError):
            r[-4]

class A:
    def __len__(self): return 3
    def __getitem__(self, i): return i


class ReversedTests(unittest.TestCase):
    def test_reversed_range(self):
        r = range(1, 10, 3)
        rr = reversed(r)
        rr0 = next(rr)
        rr1 = next(rr)
        rr2 = next(rr)
        self.assertEqual(rr2, 1)
        self.assertEqual(rr1, 4)
        self.assertEqual(rr0, 7)
        with self.assertRaises(StopIteration):
            next(rr)

    def test_reversed_list(self):
        l = [1,2,3]
        rl = list(reversed(l))
        self.assertEqual(len(l), len(rl))

    def test_reversed_custom(self):
        a = A()
        r = reversed(a)
        self.assertEqual(next(r), 2)
        self.assertEqual(next(r), 1)
        self.assertEqual(next(r), 0)
        self.assertRaises(StopIteration, next, r)

if __name__ == "__main__":
    unittest.main()
