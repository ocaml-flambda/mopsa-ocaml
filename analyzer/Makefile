TARGET_NATIVES = mopsa

# Packs
mopsa = framework lang main

framework = location exceptions ast visitor options query annotation	\
lattice zone manager flow eval post channel value domain output cache	\
analyzer lattices essentials domains config
framework.output = alarm text json factory html
framework.domains = stateless iter nonrel leaf functor stacked	\
reduced_product empty
framework.domains.reduced_product = pool reductions products factory
framework.domains.reduced_product.products = value_product domain_product

lang = universal stubs c python cfg jvm

lang.universal = ast pp visitor frontend utils zone u_stdlib router	\
iterators heap numeric strings tree # arrays
lang.universal.iterators = program intraproc interproc unittest loops
lang.universal.iterators.interproc = callstack inlining
lang.universal.heap = pool recency
lang.universal.numeric = rounding values relational relational2	\
reductions
lang.universal.numeric.values = intervals congruences float_intervals	\
integer_float_mix
lang.universal.numeric.reductions = channels intervals_congruences	\
implicit_assume_relation intervals_broadcast
lang.universal.tree = toolBox tools strVarBind SIG mapper mRelation	\
rewrite enumerator enumeratorOneFixed unionFind regularAlgebra			\
treeAlgebra abstractRegularAutomaton regExp regexpPartition					\
abstractTreeAutomaton numerical value treeAst treeAbs

lang.stubs = ast

lang.c = ast pp visitor zone frontend alarms desugar libs iterators memory
lang.c.iterators = program switch goto interproc
lang.c.desugar = loops andor aggregate record_copy stmt_expr
lang.c.memory = base init_visitor cells machine_integers
lang.c.memory.cells = cell pointer smashing expand cell2num
lang.c.libs = c_stdlib mopsa

# lang.python = ast addr utils visitor operators pp frontend
lang.python = ast addr utils visitor operators pp frontend zone		\
alarms flows libs desugar objects data_model types program memory
lang.python.data_model = callable arith_ops compare_ops aug_assign	\
attribute subscript
lang.python.types = typingdomain typing t_string t_int t_float
lang.python.memory = value_dispatcher addr_env py_string py_int py_float

lang.cfg = ast pp visitor frontend iterator iterators
lang.cfg.iterators = intraproc inlining

lang.jvm = ast precheck frontend

# Packages
PKGS = str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith javalib

# MOPSA
MOPSAROOT = ..
MOPSALIBS := utils:utils \
parsers/c:mlClangParser \
parsers/python:mlPythonParser \
parsers/universal:mlUniversalParser \
parsers/c_stubs:mlCStubsParser


# Flags
LLVMCONFIG := $(if $(shell which llvm-config), llvm-config, $(lastword $(sort $(wildcard /usr/bin/llvm-config*))))
LDFLAGS  :=$(shell $(LLVMCONFIG) --ldflags) $(LDFLAGS)

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS := -dllpath `pwd`/$(MOPSAROOT)/utils/lib -dllpath `pwd`/$(MOPSAROOT)/parsers/c/lib

include $(MOPSAROOT)/make/main.mk

# Tests
TESTS = tests

.PHONY: python-tests c-tests universal-tests

python-tests: mopsa
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.py"), \
		echo ""; \
		echo "Running test $(test)"; \
		$(MOPSAROOT)/bin/mopsa-python -unittest $(MOPSAPARAM) $(test); \
	)

c-tests: mopsa
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.c"), \
		echo ""; \
		echo "Running test $(test)"; \
		$(MOPSAROOT)/bin/mopsa-c -unittest $(MOPSAPARAM) $(test); \
	)

universal-tests: mopsa
	@ - $(foreach test, $(shell find $(TESTS) -name "*tests.u"), \
		echo ""; \
		echo "Running test $(test)"; \
		$(MOPSAROOT)/bin/mopsa-universal -unittest $(MOPSAPARAM) $(test); \
	)


tests: c-tests python-tests universal-tests
