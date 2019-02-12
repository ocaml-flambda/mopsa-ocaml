##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2019 The MOPSA Project.                                     #
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

TARGET_LIBS = mlCStubsParser

# Packs
mlCStubsParser = c_stubs_parser
c_stubs_parser = cst parser lexer ast passes main
c_stubs_parser.passes = macro_expansion predicate_expansion scoping cst_to_ast

# Packages
PKGS = zarith

# MOPSA
MOPSAROOT = ../..
MOPSALIBS := utils:utils parsers/c:mlClangParser


include $(MOPSAROOT)/make/main.mk
