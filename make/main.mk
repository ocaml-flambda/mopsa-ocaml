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

# Definitions of compilation flags, executables, etc.
-include $(MAKEROOT)/constants.mk

# Definitions of target files
-include $(MAKEROOT)/files.mk


###########
## Rules ##
###########

.PHONY: native lib-native lib-native-c clean deps merlin doc

-include $(MAKEROOT)/target_rules.mk
-include $(MAKEROOT)/ocaml_rules.mk
-include $(MAKEROOT)/pack_rules.mk
-include $(MAKEROOT)/doc_rules.mk
-include $(MAKEROOT)/c_rules.mk


ifneq ($(MAKECMDGOALS),clean)
-include $(DEPS_ML)
-include $(DEPS_MLI)
-include $(DEPS_MLL)
-include $(DEPS_MLY)
endif
