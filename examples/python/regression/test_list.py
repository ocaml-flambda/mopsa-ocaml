import sys
import pickle
import unittest

class ListTest(unittest.TestCase):

    def test_basic(self):
        self.assertEqual(list([]), [])

if __name__ == "__main__":
    unittest.main()
