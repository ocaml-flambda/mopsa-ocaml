# Makefile.  Generated from Makefile.in by configure.

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


.PHONY: all tests install clean

all:
	opam exec -- dune build --profile release -p mopsa

install:
	opam exec -- dune install --profile release

clean:
	opam exec -- dune clean -p mopsa


# Tests
.PHONY: tests universal-tests cfg-tests

TESTS = universal-tests
universal-tests.directory = analyzer/tests/universal
universal-tests.extension = u
universal-tests.analyzer = mopsa-universal

TESTS += cfg-tests
cfg-tests.directory = analyzer/tests/universal
cfg-tests.extension = u
cfg-tests.analyzer = mopsa-cfg

ifneq (,yes)
TESTS += c-tests
.PHONY: c-tests
c-tests.directory = analyzer/tests/c
c-tests.extension = c
c-tests.analyzer = mopsa-c
endif

ifneq (,yes)
TESTS += python-tests
.PHONY: python-tests
python-tests.directory = analyzer/tests/python
python-tests.extension = py
python-tests.analyzer = mopsa-python
endif


tests: $(TESTS)

$(TESTS):
	@ - $(foreach test, $(shell find $($@.directory) -name "*tests.$($@.extension)"), \
		echo ""; \
		echo "Running test $($@.analyzer) $(test)"; \
		./_build/install/default/bin/$($@.analyzer) -no-warning -unittest $(MOPSAPARAM) $(test); \
	)
