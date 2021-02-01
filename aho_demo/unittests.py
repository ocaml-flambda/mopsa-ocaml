# -*- coding: utf-8 -*-
"""
    This is part of pyahocorasick Python module.

    Unit tests for the C-based ahocorasick module.

    Author    : Wojciech Muła, wojciech_mula@poczta.onet.pl
    WWW       : http://0x80.pl/proj/pyahocorasick/
    License   : public domain
"""

# import sys
# import os
import unittest
import ahocorasick

# try:
#     import _pickle
# except ImportError:
_pickle = None


class TestCase(unittest.TestCase):
    def assertEmpty(self, collection):
        self.assertEqual(0, len(collection))


    def assertNotEmpty(self, collection):
        self.assertGreater(len(collection), 0)


class TestConstructor(TestCase):
    def test_constructor_wrong_store(self):
        with self.assertRaisesRegex(ValueError, "store value must be one of.*"):
            ahocorasick.Automaton(-42)


    def test_constructor_wrong_key_type(self):
        with self.assertRaisesRegex(ValueError, "key_type must have value.*"):
            ahocorasick.Automaton(ahocorasick.STORE_ANY, -42)


class TestTrieStorePyObjectsBase(TestCase):
    def setUp(self):
        self.A = ahocorasick.Automaton();
        self.words = "word python aho corasick \x00\x00\x00".split()
        self.inexisting = "test foo bar dword".split()


class TestTrieMethods(TestTrieStorePyObjectsBase):
    "Test basic methods related to trie structure"

    def test_empty(self):
        A = self.A
        self.assertTrue(A.kind == ahocorasick.EMPTY)
        self.assertTrue(len(A) == 0)


    def test_add_word(self):
        A = self.A
        self.assertTrue(A.kind == ahocorasick.EMPTY)

        n = 0
        for word in self.words:
            n += 1
            A.add_word(word, None)
            self.assertEqual(A.kind, ahocorasick.TRIE)
            self.assertEqual(len(A), n)

        # duplicated entry
        A.add_word(self.words[0], None)
        self.assertTrue(A.kind == ahocorasick.TRIE)
        self.assertTrue(len(A) == n)


    def test_add_empty_word(self):
        if ahocorasick.unicode:
            self.assertFalse(self.A.add_word("", None))
        else:
            self.assertFalse(self.A.add_word(b"", None))

        self.assertEqual(len(self.A), 0)
        self.assertEqual(self.A.kind, ahocorasick.EMPTY)


    def test_clear(self):
        A = self.A
        self.assertTrue(A.kind == ahocorasick.EMPTY)

        for w in self.words:
            A.add_word(w, w)

        self.assertEqual(len(A), len(self.words))

        A.clear()
        self.assertEqual(A.kind, ahocorasick.EMPTY)
        self.assertEqual(len(A), 0)


    def test_exists(self):
        A = self.A

        for w in self.words:
            A.add_word(w, w)

        for w in self.words:
            self.assertTrue(A.exists(w))

        for w in self.inexisting:
            self.assertFalse(A.exists(w))


    def test_contains(self):
        A = self.A
        for w in self.words:
            A.add_word(w, w)

        for w in self.words:
            self.assertTrue(w in A)

        for w in self.inexisting:
            self.assertTrue(w not in A)


    def test_match(self):
        A = self.A
        for word in self.words:
            A.add_word(word, word)

        prefixes = "w wo wor word p py pyt pyth pytho python \x00 \x00\x00 \x00\x00\x00".split()

        for word in prefixes:
            self.assertTrue(A.match(word))

        inexisting = "wa apple pyTon \x00\x00\x00\x00".split()
        for word in inexisting:
            self.assertFalse(A.match(word))


    def test_get1(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        for i, w in enumerate(self.words):
            self.assertEqual(A.get(w), i + 1)


    def test_get2(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        for w in self.inexisting:
            self.assertEqual(A.get(w, None), None)


    def test_get3(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        for w in self.inexisting:
            with self.assertRaises(KeyError):
                A.get(w)
        self.assertFalse(True)


    def test_get_from_an_empty_automaton(self):
        A = ahocorasick.Automaton()

        r = A.get('foo', None)
        self.assertEqual(r, None)


    def test_longest_prefix(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)
        # there is "word"
        c = A.longest_prefix("word")
        self.assertEqual(A.longest_prefix("wo"), 2)
        self.assertEqual(A.longest_prefix("working"), 3)
        self.assertEqual(A.longest_prefix("word"), 4)
        self.assertEqual(A.longest_prefix("wordbook"), 4)
        self.assertEqual(A.longest_prefix("void"), 0)
        self.assertEqual(A.longest_prefix(""), 0)



class TestTrieRemoveWord(TestTrieStorePyObjectsBase):

    def test_remove_word_from_empty_trie(self):
        self.assertFalse(self.A.remove_word("test"))


    def test_remove_existing_word(self):
        A = self.A

        words = ["he", "her", "hi", "him", "his"]
        for w in words:
            A.add_word(w, w)

        expected_len = len(A)
        for w in words:
            self.assertTrue(self.A.remove_word(w))
            self.assertFalse(self.A.exists(w))
            expected_len -= 1
            self.assertEqual(expected_len, len(A))


    def test_remove_inexisting_word(self):
        A = self.A

        words = ["he", "her", "hi", "him", "his"]
        for w in words:
            A.add_word(w, w)

        expected_len = len(A)
        for w in ["cat", "dog", "tree"]:
            self.assertFalse(self.A.exists(w))
            self.assertFalse(self.A.remove_word(w))
            self.assertEqual(expected_len, len(A))


    def test_remove__case1(self):
        words = ["k", "ki", "kit", "kitt", "kitte", "kitten"
                                 , "kitc", "kitch", "kitche", "kitchen"]

        A = self.A
        for w in words:
            A.add_word(w, w)

        expected_set = set(words)
        for w in words:
            self.assertTrue(self.A.remove_word(w))
            expected_set.discard(w)
            current_set = set(A.keys())
            self.assertEqual(expected_set, current_set)
            self.assertEqual(len(expected_set), len(A))


    def test_remove__case2(self):
        words = ["k", "ki", "kit", "kitt", "kitte", "kitten"
                                 , "kitc", "kitch", "kitche", "kitchen"]

        A = self.A
        for w in words:
            A.add_word(w, w)

        expected_set = set(words)
        for w in reversed(words):
            self.assertTrue(self.A.remove_word(w))
            expected_set.discard(w)
            current_set = set(A.keys())
            self.assertEqual(expected_set, current_set)
            self.assertEqual(len(expected_set), len(A))


    def test_remove_word_changes_type_of_automaton(self):
        A = self.A

        words = ["he", "her", "hi", "him", "his"]
        for w in words:
            A.add_word(w, w)

        A.make_automaton()
        self.assertEqual(ahocorasick.AHOCORASICK, A.kind)

        self.assertFalse(A.remove_word("inexisting"))
        self.assertEqual(ahocorasick.AHOCORASICK, A.kind)

        self.assertTrue(A.remove_word("hi"))
        self.assertEqual(ahocorasick.TRIE, A.kind)


class TestTriePop(TestTrieStorePyObjectsBase):

    def test_pop_from_empty_trie(self):
        with self.assertRaises(KeyError):
            self.A.pop("test")


    # def test_pop_existing_word(self):
    #     A = self.A

    #     words = ["he", "her", "hi", "him", "his"]
    #     for w in words:
    #         A.add_word(w, w)

    #     expected_len = len(A)
    #     for w in words:
    #         self.assertEqual(w, self.A.pop(w))
    #         self.assertFalse(self.A.exists(w))
    #         expected_len -= 1
    #         self.assertEqual(expected_len, len(A))


    # def test_pop_inexisting_word(self):
    #     A = self.A

    #     words = ["he", "her", "hi", "him", "his"]
    #     for w in words:
    #         A.add_word(w, w)

    #     expected_len = len(A)
    #     for w in ["cat", "dog", "tree"]:
    #         with self.assertRaises(KeyError):
    #             self.A.pop(w)

    #         self.assertEqual(expected_len, len(A))


    # def test_pop__case1(self):
    #     words = ["k", "ki", "kit", "kitt", "kitte", "kitten"
    #                              , "kitc", "kitch", "kitche", "kitchen"]

    #     A = self.A
    #     for w in words:
    #         A.add_word(w, w)

    #     expected_set = set(words)
    #     for w in words:
    #         self.assertEqual(w, self.A.pop(w))
    #         expected_set.discard(w)
    #         current_set = set(A.keys())
    #         self.assertEqual(expected_set, current_set)
    #         self.assertEqual(len(expected_set), len(A))


    # def test_pop__case2(self):
    #     words = ["k", "ki", "kit", "kitt", "kitte", "kitten"
    #                              , "kitc", "kitch", "kitche", "kitchen"]

    #     A = self.A
    #     for w in words:
    #         A.add_word(w, w)

    #     expected_set = set(words)
    #     for w in reversed(words):
    #         self.assertEqual(w, self.A.pop(w))
    #         expected_set.discard(w)
    #         current_set = set(A.keys())
    #         self.assertEqual(expected_set, current_set)
    #         self.assertEqual(len(expected_set), len(A))


    # def test_pop_changes_type_of_automaton(self):
    #     A = self.A

    #     words = ["he", "her", "hi", "him", "his"]
    #     for w in words:
    #         A.add_word(w, w)

    #     A.make_automaton()
    #     self.assertEqual(ahocorasick.AHOCORASICK, A.kind)

    #     with self.assertRaises(KeyError):
    #         A.pop("inexisting")

    #     self.assertEqual(ahocorasick.AHOCORASICK, A.kind)

    #     self.assertEqual("hi", A.pop("hi"))
    #     self.assertEqual(ahocorasick.TRIE, A.kind)


class TestTrieIterators(TestTrieStorePyObjectsBase):
    "Test iterators walking over trie"


    def test_iter(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        L = [word for word in A]
        K = self.words
        self.assertEqual(len(L), len(K))
        self.assertEqual(set(L), set(K))


    def test_keys(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        L = [word for word in A.keys()]
        K = [word for word in self.words]
        self.assertEqual(len(L), len(K))
        self.assertEqual(set(L), set(K))


    def test_values(self):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        L = [x for x in A.values()]
        V = list(range(1, len(self.words) + 1))
        self.assertEqual(len(L), len(V))
        self.assertEqual(set(L), set(V))


    def test_items(self):
        A = self.A
        I = []
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)
            I.append((w, i + 1))

        L = [x for x in A.items()]
        self.assertEqual(len(L), len(I))
        self.assertEqual(set(L), set(I))


    def test_items_with_prefix_valid(self):
        A = self.A
        words = "he she her hers star ham".split()
        for word in words:
            A.add_word(word, word)

        I = "he her hers".split()
        L = [x for x in A.keys("he")]
        self.assertEqual(len(L), len(I))
        self.assertEqual(set(L), set(I))


    def test_items_with_prefix_invalid(self):
        A = self.A
        words = "he she her hers star ham".split()
        for word in words:
            A.add_word(word, word)

        I = []
        L = [x for x in A.keys("cat")]
        self.assertEqual(len(L), len(I))
        self.assertEqual(set(L), set(I))


    def test_items_with_valid_pattern(self):
        A = self.A
        words = "abcde aXcd aZcdef aYc Xbcdefgh".split()
        for word in words:
            A.add_word(word, word)

        I = ["aXcd"]
        L = [x for x in A.keys("a?cd", "?")]
        self.assertEqual(set(I), set(L))


    def test_items_with_valid_pattern2(self):
        A = self.A
        words = "abcde aXcde aZcdef aYc Xbcdefgh".split()
        for word in words:
            A.add_word(word, word)

        L = [x for x in A.keys("a?c??", "?", ahocorasick.MATCH_EXACT_LENGTH)]
        I = ["abcde", "aXcde"]
        self.assertEqual(set(I), set(L))

        L = [x for x in A.keys("a?c??", "?", ahocorasick.MATCH_AT_MOST_PREFIX)]
        I = ["aYc", "abcde", "aXcde"]
        self.assertEqual(set(I), set(L))

        L = [x for x in A.keys("a?c??", "?", ahocorasick.MATCH_AT_LEAST_PREFIX)]
        I = ["abcde", "aXcde", "aZcdef"]
        self.assertEqual(set(I), set(L))


    def test_items_wrong_wildcrard(self):
        with self.assertRaisesRegex(ValueError, "Wildcard must be a single character.*"):
            self.A.keys("anything", "??")


    def test_items_wrong_match_enum(self):
        with self.assertRaisesRegex(ValueError, "The optional how third argument must be one of"):
            self.A.keys("anything", "?", -42)


class TestTrieIteratorsInvalidate(TestTrieStorePyObjectsBase):
    "Test invalidating iterator when trie is changed"

    def helper(self, method):
        A = self.A
        for i, w in enumerate(self.words):
            A.add_word(w, i + 1)

        it = method()
        w = next(it)
        # word already exists, just change associated value
        # iterator is still valid
        A.add_word(self.words[0], 2)
        w = next(it)

        # new word, iterator is invalidated
        A.add_word("should fail", 1)
        with self.assertRaises(ValueError):
            w = next(it)


    def test_keys(self):
        self.helper(self.A.keys)


    def test_values(self):
        self.helper(self.A.values)


    def test_items(self):
        self.helper(self.A.items)


class TestAutomatonBase(TestCase):
    def setUp(self):
        self.A = ahocorasick.Automaton();
        self.words = "he her hers she".split()
        self.string = "_sherhershe_"
        self.correct_positons = [
            (3, "she"),
            (3, "he"),
            (4, "her"),
            (6, "he"),
            (7, "her"),
            (8, "hers"),
            (10, "she"),
            (10, "he")
        ]


    def add_words(self):
        for word in self.words:
            self.A.add_word(word, word)

        return self.A


    def add_words_and_make_automaton(self):
        self.add_words()
        self.A.make_automaton()
        return self.A


class TestAutomatonConstruction(TestAutomatonBase):
    "Test converting trie to Aho-Corasick automaton"

    def test_make_automaton1(self):
        A = self.A
        self.assertEqual(A.kind, ahocorasick.EMPTY)
        A.make_automaton()
        # empty trie is never converted to automaton
        self.assertEqual(A.kind, ahocorasick.EMPTY)


    def test_make_automaton2(self):
        A = self.A
        self.assertEqual(A.kind, ahocorasick.EMPTY)

        self.add_words()
        self.assertEqual(A.kind, ahocorasick.TRIE)

        A.make_automaton()
        self.assertEqual(A.kind, ahocorasick.AHOCORASICK)


    def test_make_automaton3(self):
        A = self.A
        self.assertEqual(A.kind, ahocorasick.EMPTY)

        self.add_words()
        self.assertEqual(A.kind, ahocorasick.TRIE)

        A.make_automaton()
        self.assertEqual(A.kind, ahocorasick.AHOCORASICK)

        A.add_word("rollback?", True)
        self.assertEqual(A.kind, ahocorasick.TRIE)


# class TestAutomatonSearch(TestAutomatonBase):
#     "Test searching using constructed automaton (method find_all)"

#     def test_find_all1(self):
#         "no action is performed until automaton is constructed"
#         A = self.A
#         self.assertEqual(A.kind, ahocorasick.EMPTY)

#         self.assertEqual(A.find_all(self.string, "any arg"), None)

#         A.add_word("word", None)
#         self.assertEqual(A.kind, ahocorasick.TRIE)
#         self.assertEqual(A.find_all(self.string, "any arg"), None)


#     def test_find_all2(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         A.find_all(self.string, callback)

#         C = self.correct_positons
#         self.assertEqual(L, C)


#     def test_find_all3(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         start = 4
#         end = 9

#         L = []
#         A.find_all(self.string[start:end], callback)
#         C = [(pos + start, word) for pos, word in L]

#         L = []
#         A.find_all(self.string, callback, start, end)

#         self.assertEqual(L, C)


#     def test_find_all__not_a_callable_object(self):
#         A = self.add_words_and_make_automaton()

#         with self.assertRaisesRegex(TypeError, "The callback argument must be a callable such as a function."):
#             A.find_all(self.string, None)


#     def test_find_all__wrong_range__case_1(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         with self.assertRaisesRegex(IndexError, "end index not in range 0..12"):
#             A.find_all(self.string, callback, 0, len(self.string) + 5)


#     def test_find_all__wrong_range__case_2(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         with self.assertRaisesRegex(IndexError, "start index not in range 0..12"):
#             A.find_all(self.string, callback, -len(self.string) - 1, 3)


#     def test_find_all__end_index_not_given(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         A.find_all(self.string, callback, 0)


#     def test_find_all__start_is_negative(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         A.find_all(self.string, callback, -3, 4)


#     def test_find_all__end_is_negative(self):
#         A = self.add_words_and_make_automaton()

#         L = []
#         def callback(index, word):
#             L.append((index, word))

#         A.find_all(self.string, callback, 0, -1)


class TestAutomatonIterSearch(TestAutomatonBase):
    "Test searching using constructed automaton (iterator)"

    def test_iter1(self):
        A = self.A
        self.assertEqual(A.kind, ahocorasick.EMPTY)
        with self.assertRaises(AttributeError):
            A.iter(self.string)

        A.add_word("word", None)
        self.assertEqual(A.kind, ahocorasick.TRIE)
        with self.assertRaises(AttributeError):
            A.iter(self.string)


    def test_iter2(self):
        A = self.add_words_and_make_automaton()

        L = []
        for index, word in A.iter(self.string):
            L.append((index, word))

        C = self.correct_positons
        self.assertEqual(L, C)


    def test_iter3(self):
        A = self.add_words_and_make_automaton()

        start = 4
        end = 9

        C = []
        for index, word in A.iter(self.string[start:end]):
            C.append((index + start, word))

        L = []
        for index, word in A.iter(self.string, start, end):
            L.append((index, word))

        self.assertEqual(L, C)


    def test_iter_set(self):
        A = self.add_words_and_make_automaton()
        parts = "_sh erhe rshe _".split()

        expected = {
            '_sh'   : [],
            'erhe'  : [(3, 'she'),
                       (3, 'he'),
                       (4, 'her'),
                       (6, 'he')],
            'rshe'  : [(7, 'her'),
                       (8, 'hers'),
                       (10, 'she'),
                       (10, 'he')],
             '_'    : []
        }

        it = A.iter("")
        result = {}
        for part in parts:
            it.set(part)
            result[part] = []
            for item in it:
                result[part].append(item)

        self.assertEqual(expected, result)

    def test_iter_set__with_reset(self):
        A = self.add_words_and_make_automaton()

        expected = {
            'he'    : [(1, 'he')],
            'she'   : [(2, 'she'), (2, 'he')],
        }

        it = A.iter("")
        result = {}
        for part in ["he", "she"]:
            it.set(part, True)
            result[part] = []
            for item in it:
                result[part].append(item)

        self.assertEqual(expected, result)


    def test_iter_compare_with_find_all(self):
        A = self.add_words_and_make_automaton()

        # results from find_all
        L = []
        def callback(index, word):
            L.append((index, word))

        A.find_all(self.string, callback)

        # results from iterator
        C = []
        for index, word in A.iter(self.string):
            C.append((index, word))

        self.assertEqual(L, C)


    def test_iter_wrong_argument_type(self):
        A = self.add_words_and_make_automaton()

        with self.assertRaisesRegex(TypeError, "string required"):
            A.iter(None)


class TestAutomatonIterSearchWithIgnoreWhiteSpace(TestAutomatonBase):
    "Test searching using constructed automaton (iterator)"

    def setUp(self):
        self.A = ahocorasick.Automaton()
        self.words = "he her hers she".split()
        self.string = "_sh e rher she_"
        self.correct_positons = [
            (4, "she"),
            (4, "he"),
            (6, "her"),
            (8, "he"),
            (9, "her"),
            (11, "hers"),
            (13, "she"),
            (13, "he")
        ]
        self.correct_positons_start_12 = [
            (13, "he")
        ]


    def test_iter1(self):
        self.add_words_and_make_automaton()
        A = self.A
        self.assertEqual(A.kind, ahocorasick.AHOCORASICK)

        L = []
        for index, word in A.iter(self.string, ignore_white_space=True):
            L.append((index, word))
        self.assertEqual(L, self.correct_positons)


    def test_iter2(self):
        self.add_words_and_make_automaton()
        A = self.A
        self.assertEqual(A.kind, ahocorasick.AHOCORASICK)

        L = []
        for index, word in A.iter(self.string, ignore_white_space=True, start=12):
            L.append((index, word))
        self.assertEqual(L, self.correct_positons_start_12)


    def test_wrong_keyword(self):
        self.add_words_and_make_automaton()
        A = self.A
        self.assertEqual(A.kind, ahocorasick.AHOCORASICK)

        with self.assertRaises(TypeError):
            A.iter(self.string, ignore_white_space2=True)


class TestAutomatonIterInvalidate(TestAutomatonBase):
    "Test if searching iterator is invalidated when trie/automaton change"

    def test_iter1(self):
        A = self.add_words_and_make_automaton()

        it = A.iter(self.string)
        w = next(it)
        A.add_word("should fail", 1)
        with self.assertRaises(ValueError):
            w = next(it)


    def test_iter2(self):
        A = self.add_words_and_make_automaton()

        it = A.iter(self.string)
        w = next(it)
        A.clear()
        with self.assertRaises(ValueError):
            w = next(it)


class TestTrieStoreInts(TestCase):
    "Test storing plain ints as values (instead of python objects)"

    def setUp(self):
        self.A = ahocorasick.Automaton(ahocorasick.STORE_INTS);
        self.words = "word python aho corasick \x00\x00\x00".split()


    def test_add_word1(self):
        A = self.A

        # by default next values are stored
        for word in self.words:
            A.add_word(word)

        I = list(range(1, len(self.words) + 1))
        L = [A.get(word) for word in self.words]
        self.assertEqual(I, L)


    def test_add_word2(self):
        A = self.A

        # store arbitrary ints
        for i, word in enumerate(self.words):
            A.add_word(word, i + 123)

        I = list(range(123, 123 + len(self.words)))
        L = [A.get(word) for word in self.words]
        self.assertEqual(I, L)


    def test_add_word3(self):
        # not a number
        with self.assertRaises(TypeError):
            self.A.add_word("xyz", None)


    def test_iter(self):
        A = self.A
        for word in self.words:
            A.add_word(word);

        I = set(range(1, len(A) + 1))
        L1 = [val for val in A.values()]
        L2 = [val for key, val in A.items()]

        self.assertEqual(L1, L2)
        self.assertEqual(set(L1), I)


    def test_find_all_and_iter(self):
        words = "he her hers she".split()
        string = "_sherhershe_"

        A = self.A
        for word in words:
            A.add_word(word)

        A.make_automaton()

        # find_all()
        C = []
        def callback(index, value):
            C.append((index, value))

        A.find_all(string, callback);

        # iter()
        L = [(index, value) for index, value in A.iter(string)]

        #
        self.assertEqual(C, L)


class TestTrieStoreLengths(TestCase):
    """Test storing plain ints -- length of words --- as values
    (instead of python objects)"""

    def setUp(self):
        self.A = ahocorasick.Automaton(ahocorasick.STORE_LENGTH);
        self.words = "word python aho corasick \x00\x00\x00".split()


    def test_add_word1(self):
        A = self.A

        # by default next values are stored
        for word in self.words:
            A.add_word(word)

        for key, value in A.items():
            self.assertEqual(len(key), value)


class TestBugAutomatonSearch(TestAutomatonBase):
    """Bug in search"""

    def setUp(self):
        self.A = ahocorasick.Automaton()
        self.words = ['GT-C3303', 'SAMSUNG-GT-C3303K/']


    def test_bug(self):
        self.add_words_and_make_automaton()
        text = 'SAMSUNG-GT-C3303i/1.0 NetFront/3.5 Profile/MIDP-2.0 Configuration/CLDC-1.1'

        res = list(self.A.iter(text))

        self.assertEqual([(15, 'GT-C3303')], res)


class TestIntSequenceBase(TestCase):
    def setUp(self):
        self.A = ahocorasick.Automaton(ahocorasick.STORE_ANY, ahocorasick.KEY_SEQUENCE);


class TestIntSequence__TrieMethods(TestIntSequenceBase):

    def test_add__case_1(self):
        A = self.A

        ret = A.add_word((1, 2, 3), "foo")
        self.assertTrue(ret)
        self.assertTrue(A.kind == ahocorasick.TRIE)

        self.assertEqual(len(A), 1)
        self.assertTrue((1, 2, 3) in A)


    def test_add__case_2(self):
        A = self.A

        A.add_word((1, 2, 3), "foo")
        ret = A.add_word((1, 2, 3), "bar")
        self.assertFalse(ret)


    def test_add__case_3(self):
        A = self.A

        A.add_word((1, 2, 3), "foo")
        A.add_word((1, 2, 3, 4, 5), "bar")
        A.add_word((1, 3, 4, 5), "baz")

        self.assertEqual(len(A), 3);
        self.assertEqual(A.get((1, 2, 3)),          "foo");
        self.assertEqual(A.get((1, 2, 3, 4, 5)),    "bar");
        self.assertEqual(A.get((1, 3, 4, 5)),       "baz");


    def test_add__case_4(self):
        A = self.A

        ret = A.add_word((), "foo")
        self.assertFalse(ret)


    def test_add__case_5__wrong_argument_type(self):
        A = self.A

        with self.assertRaises(TypeError) as e:
            A.add_word("hello!", "foo")

        self.assertEqual(str(e.exception), "argument is not a supported sequence type")


    def test_add__case_6__wrong_item_type(self):
        A = self.A

        with self.assertRaises(ValueError) as e:
            A.add_word((1, 2, "hello!"), "foo")

        self.assertEqual(str(e.exception), "item #2 is not a number")


    def test_add__case_7__wrong_value(self):
        A = self.A

        with self.assertRaises(ValueError) as e:
            A.add_word((1, -1, 12), "foo")

        errmsg = str(e.exception)
        msgs = [
            "item #1: value -1 outside range [0..65535]",
            "item #1: value -1 outside range [0..4294967295]",
        ]

        self.assertIn(errmsg, msgs)


    def test_add__case_8__wrong_value(self):
        A = self.A

        with self.assertRaises(ValueError) as e:
            A.add_word((2**42, 0, 12), "foo")

        # Depending on python's version the message might be different,
        # but the type remains the same.

        errmsg = str(e.exception)
        msgs = [
            "item #0: value 4398046511104 outside range [0..65535]",
            "item #0: value 4398046511104 outside range [0..4294967295]",
            "item #0 is not a number",
        ]

        self.assertIn(errmsg, msgs)


    def test_match(self):
        A = self.A

        ret = A.add_word((1, 2, 3), "foo")
        self.assertTrue(A.match((1,)))
        self.assertTrue(A.match((1, 2)))
        self.assertTrue(A.match((1, 2, 3)))


    def test_longest_prefix(self):
        A = self.A

        ret = A.add_word((1, 2, 3, 4, 5, 6), "foo")
        self.assertEqual(A.longest_prefix((1, 2, 3, 111, 1111, 11111)), 3);
        self.assertEqual(A.longest_prefix((111, 1111, 11111)), 0);

    def test_iter1(self):
        A = self.A

        A.add_word((1, 2, 3), "foo")
        A.add_word((2, 3, 4, 5), "bar")
        A.add_word((2, 3, 5), "baz")
        A.make_automaton()

        L = [(index, value) for index, value in A.iter((1, 2, 3, 5))]

        self.assertEqual(L, [
            (2, "foo"),
            (3, "baz"),
        ])

    def test_iter2(self):
        A = self.A

        A.add_word((43, 89), (43, 89))
        A.add_word((43, 89, 64), (43, 89, 64))
        A.add_word((89, 64), (89, 64))
        A.add_word((89, 100), (89, 100))
        A.make_automaton()

        L = [
            (index, value)
            for index, value in
            A.iter((80, 80, 43, 89, 90, 89, 64, 100, 43, 89, 100))
        ]

        self.assertEqual(L, [
            (3, (43, 89)),
            (6, (89, 64)),
            (9, (43, 89)),
            (10, (89, 100)),
        ])


    def test_iter_wrong_argument_type(self):
        A = self.A
        A.add_word((89, 100), (89, 100))
        A.make_automaton()

        with self.assertRaisesRegex(TypeError, "tuple required"):
            self.A.iter(None)


# class TestDump(TestAutomatonBase):
#     def test_dump_empty(self):
#         self.assertIsNone(self.A.dump())


#     def test_dump_trie(self):
#         self.add_words()
#         ret = self.A.dump()

#         self.assertEqual(3, len(ret))
#         self.assertNotEmpty(ret[0])     # list of nodes
#         self.assertNotEmpty(ret[1])     # list of edges
#         self.assertEmpty(ret[2])        # list of fail links -- empty, if not an automaton


#     def test_dump_automaton(self):
#         self.add_words_and_make_automaton()
#         ret = self.A.dump()

#         self.assertEqual(3, len(ret))
#         self.assertNotEmpty(ret[0])     # list of nodes
#         self.assertNotEmpty(ret[1])     # list of edges
#         self.assertNotEmpty(ret[2])     # list of fail links


class TestIssue53(TestCase):
    """
    Problems with handling of UCS-2 encoding
    """

    def test_case1(self):
        # test contributed by @woakesd (David Woakes)

        a = ahocorasick.Automaton()
        a.add_word('test', 'test')

        a.make_automaton()

        test_string = 'test 🙈 test?!'

        # wrongly calculated matching position
        for item in a.iter(test_string):
            start = item[0] - len(item[1]) + 1
            match = test_string[start:item[0] + 1]
            self.assertEqual(match, "test")


    def test_case2(self):
        a = ahocorasick.Automaton()
        a.add_word('test', 'test')

        a.make_automaton()

        test_string = '🙈' * 1000

        # wrongly calculated the input's length
        for item in a.iter(test_string):
            pass



class TestLongIterString(TestAutomatonBase):
    def test_match(self):
        A = ahocorasick.Automaton();
        for word in "he here her".split():
            A.add_word(word, word)

        A.make_automaton()

        result = list(A.iter_long("he here her"))
        self.assertEqual(result[0], (1, "he"))
        self.assertEqual(result[1], (6, "here"))
        self.assertEqual(result[2], (10, "her"))


class TestLongIterSequence(TestAutomatonBase):
    def test_match(self):
        A = ahocorasick.Automaton(ahocorasick.STORE_ANY, ahocorasick.KEY_SEQUENCE);
        for word in [(1, 2), (1, 2, 3), (1, 2, 3, 4)]:
            A.add_word(word, word)

        A.make_automaton()

        result = list(A.iter_long((0, 1, 2, 3, 4, 0, 0, 1, 2, 0, 1, 3, 1, 2, 3, 0)))
        #                             ^^^^^^^^^^        ^^^^           ^^^^^^^
        #                                index 4           8                14
        self.assertEqual(result[0], (4, (1, 2, 3, 4)))
        self.assertEqual(result[1], (8, (1, 2)))
        self.assertEqual(result[2], (14, (1, 2, 3)))


if __name__ == '__main__':
    unittest.main()
