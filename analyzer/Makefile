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
mopsa = framework mopsa langs main

# Framework
framework = core export engine lattices domains composers config
framework.core = ast visitor eq query annotation lattice zone manager flow eval \
		 post channel value domain callstack alarm
framework.export = text json factory
framework.engine = logging cache analyzer
framework.lattices = finite_powerset partial_map powerset total_map
framework.composers = iter stacked reduced_product
framework.domains = stateless nonrel leaf functor empty
framework.config = setup abstraction options
framework.composers.reduced_product = pool reductions products factory
framework.composers.reduced_product.products = value_product domain_product

# Languages
langs = universal stubs c python

# Universal
langs.universal = ast frontend zone u_stdlib iterators heap numeric strings
langs.universal.iterators = program intraproc interproc unittest loops
langs.universal.iterators.interproc = inlining
langs.universal.heap = pool recency
langs.universal.numeric = rounding values relational
langs.universal.numeric.values = intervals congruences float_intervals	\
integer_float_mix
langs.universal.numeric.reductions = channels intervals_congruences	\
implicit_assume_relation intervals_broadcast

# Stubs
langs.stubs = ast zone alarms iterator

# C
langs.c = ast pp visitor zone frontend alarms desugar memory libs iterators resources
langs.c.iterators = program switch goto interproc
langs.c.desugar = low_level record_copy loops stub_init cond_expr
langs.c.memory = common cells pointers machine_numbers
langs.c.memory.common = base points_to init_visitor
langs.c.memory.cells = cell expand cell2scalar
langs.c.memory.pointers = bases nonrel
langs.c.libs = libmopsa libc
langs.c.resources = common file
langs.c.resources.file = slot table domain

# Python
langs.python = ast addr utils visitor operators pp frontend zone		\
alarms  libs desugar objects data_model types flows program
langs.python.data_model = callable arith_ops compare_ops aug_assign	\
attribute subscript
langs.python.types = typing addr_env t_string t_int t_float t_list t_set


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
