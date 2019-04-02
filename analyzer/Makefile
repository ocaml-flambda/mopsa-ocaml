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
framework = ast core lattices engines operators output config
framework.ast = program typ operator constant var expr stmt visitor all
framework.core = query context zone token log lattice flow eval post manager \
	         id interface sig callstack alarm all
framework.core.sig = lowlevel unified simplified
framework.lattices = partial_map powerset total_map
framework.engines = logging cache abstraction engine automatic interactive
framework.output = text json factory
framework.operators = apply sequence compose nonrel
framework.config = paths abstraction options

# Languages
lang = universal stubs c python

# Universal
lang.universal = ast frontend
lang.universal = ast frontend zone iterators heap numeric
lang.universal.iterators = program intraproc interproc unittest loops
lang.universal.iterators.interproc = inlining
lang.universal.heap = pool recency
lang.universal.numeric = rounding values relational
lang.universal.numeric.values = integer_interval

# Stubs
lang.stubs = ast zone alarms iterator

# C
lang.c = ast pp visitor zone frontend alarms desugar memory libs iterators resources
lang.c.iterators = program switch goto interproc
lang.c.desugar = low_level record_copy loops stub_init cond_expr
lang.c.memory = common cells pointers machine_numbers
lang.c.memory.common = base points_to init_visitor
lang.c.memory.cells = cell expand cell2scalar
lang.c.memory.pointers = bases nonrel
lang.c.libs = libmopsa libc
lang.c.resources = common file
lang.c.resources.file = slot table domain

# Python
lang.python = ast addr utils visitor operators pp frontend
# lang.python = ast addr utils visitor operators pp frontend zone alarms libs \
# 	      desugar objects data_model types flows program
# lang.python.data_model = callable arith_ops compare_ops aug_assign attribute \
# 	                 subscript
# lang.python.types = typing addr_env t_string t_int t_float t_list t_set


# Packages
PKGS = str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith


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
