###############################################################################
#                                                                             #
#  Common makefile rules for compiling OCaml sources with auto conversion     #
#  of directories into packs.                                                 #
#                                                                             #
#                                                                             #
#  Copyright (C) 2017 The MOPSA Project                                       #
#                                                                             #
#  This program is free software: you can redistribute it and/or modify       #
#  it under the terms of the CeCILL license V2.1.                             #
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
