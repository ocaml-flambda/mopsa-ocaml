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

###############################################################################
#                                                                             #
#  Common makefile rules for compiling OCaml sources with auto conversion     #
#  of directories into packs.                                                 #
#                                                                             #
###############################################################################


###############
# Definitions #
###############

# Root directory of MOPSA
MAKEROOT=$(MOPSAROOT)/make

# Definition of utility functions
-include $(MAKEROOT)/functions.mk

# Definitions of compilation flags, executables, etc.
-include $(MAKEROOT)/constants.mk

# Definitions of target files
-include $(MAKEROOT)/files.mk


###########
## Rules ##
###########

.PHONY: native lib-native lib-native-c clean deps merlin doc

-include $(MAKEROOT)/target.mk
-include $(MAKEROOT)/flags.mk
-include $(MAKEROOT)/ocaml.mk
-include $(MAKEROOT)/merlin.mk
-include $(MAKEROOT)/c.mk


ifneq ($(MAKECMDGOALS),clean)
-include $(ML:$(SRC)/%=$(BUILD)/%.dep)
-include $(MLI:$(SRC)/%=$(BUILD)/%.dep)
-include $(MLL:$(SRC)/%.mll=$(BUILD)/%.ml.dep)
-include $(MLY:$(SRC)/%.mly=$(BUILD)/%.mli.dep)
-include $(MLY:$(SRC)/%.mly=$(BUILD)/%.ml.dep)
endif
