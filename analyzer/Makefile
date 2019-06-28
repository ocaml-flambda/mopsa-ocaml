##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2019 The MOPSA Project.                                #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################


TARGET_NATIVES = mopsa

# Packs
mopsa = framework mopsa lang main

# Framework
framework = ast core lattices engines transformers output config
framework.ast = program typ operator constant var expr stmt visitor all
framework.core = query context zone token log lattice flow result eval post \
		 id interface channel sig callstack alarm debug_tree cache \
		 abstraction all
framework.core.sig = value domain stacked
framework.core.sig.value = lowlevel simplified reduction
framework.core.sig.domain = manager lowlevel intermediate simplified stateless reduction
framework.core.sig.stacked = manager lowlevel intermediate stateless reduction
framework.lattices = partial_map powerset
framework.engines = engine automatic interactive
framework.transformers = value domain stacked
framework.output = text json factory
framework.config = paths visitor typer parser options

lang = universal repl stubs c python

# Universal
lang.universal = ast frontend
lang.universal = ast frontend zone iterators heap numeric
lang.universal.iterators = program intraproc scalar interproc unittest loops
lang.universal.iterators.interproc = inlining sequential_cache join_based_cache
lang.universal.heap = policies recency
lang.universal.numeric = rounding common values relational reductions
lang.universal.numeric.values = intervals congruences
lang.universal.numeric.values.intervals = integer float
lang.universal.numeric.relational = apron_manager binding apron_transformer factory octagon polyhedra default
lang.universal.numeric.reductions = intervals_congruences intervals

lang.repl = frontend

lang.stubs = ast zone alarms iterator

# C
lang.c = ast pp visitor zone frontend alarms desugar memory libs iterators
lang.c.iterators = program switch loops goto interproc
lang.c.desugar = cond_expr
lang.c.memory = common structured lowlevel scalars
lang.c.memory.common = base points_to quantified_offset
lang.c.memory.structured = flat
lang.c.memory.lowlevel = cells string_length pointer_sentinel reductions
lang.c.memory.scalars = pointers machine_numbers
lang.c.memory.scalars.pointers = value symbolic domain
lang.c.libs = builtins clib mopsalib compiler variadic cstubs
lang.c.libs.clib.file_descriptor = slot table domain



# Python
lang.python = ast addr utils visitor  operators pp ast_compare frontend zone		\
              alarms libs desugar objects data_model types flows program
lang.python.objects = class function range nones py_list tuple dict py_set
lang.python.data_model = callable arith_ops compare_ops aug_assign	\
                         attribute subscript
lang.python.types = typing addr_env typechecking t_string t_int t_float t_list t_set t_complex


# Packages
PKGS = str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith threads


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
