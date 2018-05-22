TARGET_NATIVES = mopsa

MOPSAROOT = ..

#Packs
mopsa = framework lang main

framework = options ast query pp alarm context lattice lattices visitor exceptions flow eval manager exec utils domains analyzer config
framework.lattices = enum top_set partial_map total_map range_set
framework.domains = stateful stateless reduce fun iter unify reduce_unify
framework.domains.reduce = domain reduction root product
framework.domains.unify = domain root
framework.domains.reduce_unify = domain root product

lang = universal c python

lang.universal = ast pp visitor utils flows heap nonrel numeric unit_tests setup
lang.universal.heap = pool recency
lang.universal.nonrel = value domain
lang.universal.numeric = values query reduction domains
lang.universal.numeric.values = int congruence float
lang.universal.numeric.domains = boxes relational
lang.universal.numeric.domains.boxes = int congruence float

lang.python = ast addr pp alarm utils visitor frontend builtins operators memory desugar objects flows libs program setup
lang.python.memory = value nonrel
lang.python.objects = class function object number strings containers data_model
lang.python.objects.containers = lists dicts ranges sets tuples
lang.python.objects.data_model = attribute callable arith_ops subscript compare_ops
lang.python.flows = exceptions

lang.c = ast frontend pp visitor alarms program flows desugar libs memory setup
lang.c.memory = base query pointer init cell machine_integers access_path structured
lang.c.memory.cell = initzero expand smashing reductions
lang.c.flows = goto interproc switch

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

tests: c-tests python-tests
