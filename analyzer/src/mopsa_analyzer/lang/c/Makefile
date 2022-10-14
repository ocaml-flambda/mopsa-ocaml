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



# C packs
mopsa_analyzer.lang += c
mopsa_analyzer.lang.c = ast pp visitor frontend common soundness libs iterators cstubs memory \
	 hooks packing
mopsa_analyzer.lang.c.common = builtins base alarms points_to quantified_offset scope_update
mopsa_analyzer.lang.c.iterators = program switch loops goto interproc intraproc
mopsa_analyzer.lang.c.memory = aggregates variable_length_array protection scalars lowlevel \
	        reductions stackframe
mopsa_analyzer.lang.c.memory.lowlevel = cells string_length smashing pointer_sentinel
mopsa_analyzer.lang.c.memory.scalars = pointers machine_numbers
mopsa_analyzer.lang.c.memory.scalars.pointers = value static_points_to domain
mopsa_analyzer.lang.c.memory.reductions = pointer_eval
mopsa_analyzer.lang.c.cstubs = aux_vars resources builtins assigns
mopsa_analyzer.lang.c.libs = libc libmopsa compiler_builtins
mopsa_analyzer.lang.c.libs.libc.formatted_io = placeholder parser lexer format_string fscanf fprintf
mopsa_analyzer.lang.c.libs.libc.file_descriptor = slot table domain
mopsa_analyzer.lang.c.hooks = disj analysis_bugs coverage
mopsa_analyzer.lang.c.packing = static_scope



# MOPSA
MOPSALIBS += parsers/c:MopsaClangParser \
             parsers/c_stubs:MopsaCStubsParser


# Flags
LDFLAGS += $(shell $(LLVMCONFIG) --ldflags)

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/parsers/c/lib
