MOPSAROOT = ..
TARGET = mopsa

all: native


#Packs
mopsa = framework lang main

framework = options ast query pp alarm context lattice lattices visitor flow manager domains utils analyzer config
framework.lattices = enum top_set partial_map total_map range_set
framework.domains = global exec eval local localFlow stateless composers


lang = universal c python

lang.universal = ast pp visitor utils flows heap nonrel numeric unit_tests setup
lang.universal.heap = pool recency
lang.universal.nonrel = value domain
lang.universal.numeric = interval query integers floats relational
lang.universal.numeric.integers = value domain
lang.universal.numeric.floats = value domain

lang.python = ast addr pp alarm utils visitor frontend builtins memory desugar objects libs program setup
lang.python.memory = value nonrel
lang.python.objects = class function object data_model
lang.python.objects.data_model = attribute callable

lang.c = ast frontend pp visitor memory flows libs program setup
lang.c.memory = cell pointer machine_integers

#Packages
PKGS = str yojson apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith

#Flags
INCLUDES = -I $(MOPSAROOT)/utils/bin -I $(MOPSAROOT)/parsers/c/bin -I $(MOPSAROOT)/parsers/python/bin
LIBCMXA = utils.cmxa mlClangParser.cmxa mlPythonParser.cmxa

include $(MOPSAROOT)/make/main.mk

#tests
TESTS = tests

python-tests: native
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.py"), \
		echo ""; \
		echo "Running test $(test)"; \
		./bin/mopsa.native -test true -config configs/default.py.json -stubs stubs/python $(MOPSAPARAM) $(test); \
	)

c-tests: native
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.c"), \
		echo ""; \
		echo "Running test $(test)"; \
		./bin/mopsa.native -test true -config configs/default.c.json -stubs stubs/c $(MOPSAPARAM) $(test); \
	)

tests: python-tests c-tests
