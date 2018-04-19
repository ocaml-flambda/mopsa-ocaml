TARGET_NATIVES = mopsa

MOPSAROOT = ..

#Packs
mopsa = framework lang main

framework = options ast query pp alarm context lattice lattices visitor exceptions flow eval manager exec utils domains analyzer config
framework.lattices = enum top_set partial_map total_map range_set
framework.domains = stateful stateless reduction fun iter
framework.domains.reduction = domain root product


lang = universal c python

lang.universal = ast pp visitor utils flows heap nonrel numeric unit_tests setup
lang.universal.heap = pool recency
lang.universal.nonrel = value domain
lang.universal.numeric = values query reduction domains
lang.universal.numeric.values = int congruence float
lang.universal.numeric.domains = boxes relational
lang.universal.numeric.domains.boxes = int congruence float

lang.python = ast addr pp alarm utils visitor frontend builtins memory desugar objects libs program setup
lang.python.memory = value nonrel
lang.python.objects = class function object data_model
lang.python.objects.data_model = attribute callable

lang.c = ast frontend pp visitor alarms program memory flows desugar libs setup
lang.c.memory = cell pointer machine_integers array_to_pointer record_to_pointer var_init

#Packages
PKGS = str yojson apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith

#Flags
INCLUDES = -I $(MOPSAROOT)/utils/lib -I $(MOPSAROOT)/parsers/c/lib -I $(MOPSAROOT)/parsers/python/lib
LIBCMXA = utils.cmxa mlClangParser.cmxa mlPythonParser.cmxa

LLVMCONFIG := $(if $(shell which llvm-config), llvm-config, $(lastword $(sort $(wildcard /usr/bin/llvm-config*))))
LDFLAGS  :=$(shell $(LLVMCONFIG) --ldflags) $(LDFLAGS)


include $(MOPSAROOT)/make/main.mk

#tests
TESTS = tests

.PHONY: python-tests c-tests

python-tests: mopsa
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.py"), \
		echo ""; \
		echo "Running test $(test)"; \
		$(MOPSAROOT)/bin/mopsa -lang=python -test true -config configs/default.py.json -stub stubs/python $(MOPSAPARAM) $(test); \
	)

c-tests: mopsa
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.c"), \
		echo ""; \
		echo "Running test $(test)"; \
		$(MOPSAROOT)/bin/mopsa -lang=c -test true -config configs/default.c.json -stub stubs/c $(MOPSAPARAM) $(test); \
	)

tests: python-tests c-tests
