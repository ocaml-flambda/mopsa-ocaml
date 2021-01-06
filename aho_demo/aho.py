import ahocorasick

a = ahocorasick.Automaton()
assert(a.kind == ahocorasick.EMPTY)
assert(len(a) == 0)
a.add_word("abc", None)
assert(a.kind == ahocorasick.TRIE)
assert(len(a) == 1)
