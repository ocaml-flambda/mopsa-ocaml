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

SV_COMP_FILES = bin/mopsa.bin bin/mopsa bin/mopsa-c bin/mopsa-sv-comp share/mopsa/configs/c/*.json share/mopsa/stubs/c/ LICENSE* README.md
OPAM_BIN = $(shell which ocaml)
OPAM_ROOT = $(realpath $(shell dirname $(OPAM_BIN))/..)

sv-comp: all
	rm -rIf mopsa mopsa.zip
	mkdir mopsa
	cp $(OPAM_ROOT)/share/apron/lib/libapron.so mopsa/
	cp $(OPAM_ROOT)/share/apron/lib/libpolkaMPQ.so mopsa/
	cp $(OPAM_ROOT)/share/apron/lib/liboctMPQ.so mopsa/
	cp -L -r --parents $(SV_COMP_FILES) mopsa/
	sed -i '5 i export LD_LIBRARY_PATH=$$(realpath $${MOPSADIR}):$${LD_LIBRARY_PATH}' ./mopsa/bin/mopsa
	zip -r mopsa.zip mopsa

TMP := $(shell mktemp -d)
sv-comp-test: sv-comp
  # The mopsa machine has a compatible libc version
	unzip -d $(TMP) mopsa.zip
	sudo docker pull registry.gitlab.com/sosy-lab/benchmarking/competition-scripts/user:latest
	sudo docker run --rm -i -t --volume=$(TMP):/tool --workdir=/tool registry.gitlab.com/sosy-lab/benchmarking/competition-scripts/user:latest bash
	rm -rI $(TMP)

tests: $(TESTS)

$(TESTS):
	@ - $(foreach test, $(shell find $($@.directory) -name "*tests.$($@.extension)"), \
		echo ""; \
		echo "Running test $($@.analyzer) $(test)"; \
		./_build/install/default/bin/$($@.analyzer) -no-warning -unittest $(MOPSAPARAM) $(test); \
	)
