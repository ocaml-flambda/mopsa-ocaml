import ahocorasick

a = ahocorasick.Automaton()
assert(a.kind == ahocorasick.EMPTY)
b = len(a)
# assert(len(a) == 0)
a.add_word("abc", None)
b = len(a)
c = a.kind
assert(a.kind == ahocorasick.TRIE)
