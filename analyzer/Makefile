TARGET_NATIVES = mopsa

MOPSAROOT = ..

#Packs
mopsa = framework lang main

framework = location ast visitor options query annotation alarm lattice zone manager flow exceptions eval post channel value domain cache analyzer lattices essentials domains config
framework.domains = stateless stateless_zoned iter nonrel leaf functor stacked reduced_product empty router
framework.domains.reduced_product = pool reductions products factory
framework.domains.reduced_product.products = value_product domain_product

lang = universal c python

lang.universal = ast pp visitor frontend utils zone iterators numeric arrays
lang.universal.iterators = program intraproc loops interproc
lang.universal.numeric = values relational reductions
lang.universal.numeric.values = intervals congruences
lang.universal.numeric.reductions = channels intervals_congruences implicit_assume_relation intervals_broadcast

lang.c = zone ast alarms frontend libs desugar flows memory

lang.c = zone ast pp visitor frontend alarms desugar libs iterators memory
lang.c.iterators = program switch goto interproc
lang.c.desugar = loops andor aggregate
lang.c.memory = base init_visitor cells machine_integers
lang.c.memory.cells = cell pointer smashing expand router cell2num
lang.c.libs = c_stdlib

lang.python = ast addr pp frontend zone libs desugar program types
lang.python.types = typingdomain typing t_string t_int


#Packages
PKGS = str yojson apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith

#Flags
INCLUDES = -I $(MOPSAROOT)/utils/lib -I $(MOPSAROOT)/parsers/c/lib -I $(MOPSAROOT)/parsers/python/lib -I $(MOPSAROOT)/parsers/universal/lib
LIBCMXA = utils.cmxa mlClangParser.cmxa mlPythonParser.cmxa mlUniversalParser.cmxa

LLVMCONFIG := $(if $(shell which llvm-config), llvm-config, $(lastword $(sort $(wildcard /usr/bin/llvm-config*))))
LDFLAGS  :=$(shell $(LLVMCONFIG) --ldflags) $(LDFLAGS)


include $(MOPSAROOT)/make/main.mk

#tests
TESTS = tests

.PHONY: python-tests c-tests universal-tests

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

universal-tests: mopsa
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.u"), \
		echo ""; \
		echo "Running test $(test)"; \
		$(MOPSAROOT)/scripts/mopsa-universal -test=true $(MOPSAPARAM) $(test); \
	)


tests: c-tests python-tests universal-tests
