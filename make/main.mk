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

.PHONY: native lib-native lib-native-c clean deps merlin tests tests-deps doc

-include $(MAKEROOT)/target_rules.mk
-include $(MAKEROOT)/pack_rules.mk
-include $(MAKEROOT)/dep_rules.mk
-include $(MAKEROOT)/ocaml_rules.mk
-include $(MAKEROOT)/doc_rules.mk
-include $(MAKEROOT)/parser_rules.mk
-include $(MAKEROOT)/c_rules.mk
-include $(MAKEROOT)/test_rules.mk


ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),tests)
$(info Generating packs templates)
$(foreach pack,$(PACKS),$(eval $(call PACK_template,$(pack))))
-include $(BML:%.ml=%.dep)
-include $(BMLY:%.mly=%.ydep)
endif
endif

ifeq ($(MAKECMDGOALS),tests)
-include $(TESTS_DEP)
endif
