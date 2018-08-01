TARGET_NATIVES = mopsa

MOPSAROOT = ..

#Packs
mopsa = framework lang main

framework = location ast visitor options query annotation alarm lattice zone manager flow exceptions eval post domain analyzer lattices essentials domains config
framework.domains = stateless iter nonrel leaf
framework.domains.nonrel = value factory value_reduced_product

lang = universal

lang.universal = ast pp visitor frontend zone iterators numeric
lang.universal.iterators = program intraproc loops
lang.universal.numeric = values relational reductions
lang.universal.numeric.values = integer_interval integer_congruence


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
