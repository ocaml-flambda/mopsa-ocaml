import unittest

class Counter:
    def __init__(self, x):
        self.up = max(0, x)
        self.pos = 0

    def __iter__(self):
        return self

    def __next__(self):
        p = self.pos
        if p < self.up:
            self.pos += 1
            return p
        else:
            raise StopIteration

class ListTests(unittest.TestCase):
    def test_initialization_with_same_values(self):
        l = [1, 1]
        x = l[0]
        self.assertEqual(x, 1)

    def test_len_of_initialization(self):
        l = [1, 1]
        x = len(l)
        self.assertEqual(x, 2)

    def test_add(self):
        l1 = [1, 2, 3]
        l2 = l1 + [4, 5]
        n = len(l2)
        self.assertEqual(n, 5)

    def test_list_slice(self):
        l1 = [1, 2, 3, 4]
        l2 = l1[1:3]
        self.assertEqual(len(l2), 2)

    def test_in(self):
        l1 = [1, 1, 1]
        self.assertTrue(1 in l1)
        l2 = [1, 2, 3]
        self.assertTrue(2 in l2)

    def test_extend(self):
        l = [1,2,3]
        l.extend(range(6))
        self.assertTrue(4 in l)
        self.assertEqual(len(l), 9)
        self.assertRaises(TypeError, l.extend, 3)
        l2 = []
        c = Counter(10)
        l2.extend(c)
        self.assertEqual(len(l2), 10)
        self.assertTrue(0 in l2)
        self.assertTrue(9 in l2)

if __name__ == "__main__":
    unittest.main()
