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
lang += c
lang.c = ast pp visitor zone frontend common desugar libs iterators cstubs \
         memory hooks packing
lang.c.common = builtins base alarms points_to quantified_offset scope_update
lang.c.iterators = program switch loops goto interproc
lang.c.desugar = conditions constants aggregates
lang.c.memory = variable_length_array lowlevel scalars
lang.c.memory.lowlevel = cells string_literal string_length smashing \
                         pointer_sentinel reductions
lang.c.memory.lowlevel.reductions = cell_string_literal cell_string_length \
                                    cell_pointer_sentinel cell_smashing
lang.c.memory.scalars = pointers machine_numbers
lang.c.memory.scalars.pointers = value static_points_to domain
lang.c.cstubs = aux_vars resources assigns requires builtins
lang.c.libs = libc libmopsa compiler_builtins
lang.c.libs.libc.formatted_io = placeholder parser lexer format_string fscanf fprintf
lang.c.libs.libc.file_descriptor = slot table domain
lang.c.hooks = disj analysis_bugs coverage
lang.c.packing = static_scope reductions



# MOPSA
MOPSALIBS += parsers/c:mlClangParser \
             parsers/c_stubs:mlCStubsParser


# Flags
LDFLAGS += $(shell $(LLVMCONFIG) --ldflags)

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/parsers/c/lib
