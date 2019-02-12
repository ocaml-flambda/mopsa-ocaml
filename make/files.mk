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

# Clean source files. Excludes temporary Emacs files starting and ending with '#'.
FILES = $(shell find $(SRC) -name "*.*" | grep -v "\#*\#")

# OCaml source files
ML  = $(filter %.ml,$(FILES))
MLI = $(filter %.mli,$(FILES))
MLL = $(filter %.mll,$(FILES))
MLY = $(filter %.mly,$(FILES))

# OCaml packs are all directories within $(SRC) hierarchy.
PACKS = $(patsubst $(SRC)/%,%,$(shell find $(SRC)/* -type d))

ML_AUTOGEN = $(MLL:$(SRC)/%.mll=$(BUILD)/%.ml) $(MLY:$(SRC)/%.mly=$(BUILD)/%.ml) $(PACKS:%=$(BUILD)/%.ml)
MLI_AUTOGEN = $(MLY:$(SRC)/%.mly=$(BUILD)/%.mli)

## Libraries
LIBCMXA = $(LIBS:%=%.cmxa) $(foreach lib,$(MOPSALIBS),$(call lib_file,$(lib)).cmxa)
LIBCMA  = $(LIBS:%=%.cma) $(foreach lib,$(MOPSALIBS),$(call lib_file,$(lib)).cma)

## Merlin files
MERLIN = $(SRC)/.merlin $(PACKS:%=$(SRC)/%/.merlin)

## C/C++ sources
C_OBJ  = $(C_SRC:%.c=$(BUILD)/%.o)
CC_OBJ = $(CC_SRC:%.cc=$(BUILD)/%.o)
