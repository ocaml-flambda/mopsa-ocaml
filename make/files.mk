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
