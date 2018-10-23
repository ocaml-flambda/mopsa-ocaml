# Clean source files, excluding temporary files starting and ending with '#'
FILES = $(shell find $(SRC) -name "*.*" | grep -v "\#*\#")

# OCaml source files
ML  = $(filter %.ml,$(FILES))
MLI = $(filter %.mli,$(FILES))
MLL = $(filter %.mll,$(FILES))
MLY = $(filter %.mly,$(FILES))

# OCaml packs
PACKS = $(patsubst $(SRC)/%,%,$(shell find $(SRC)/* -type d))

## Libraries
LIBCMXA = $(LIBS:%=%.cmxa) $(foreach lib,$(MOPSALIBS),$(call lib_file,$(lib)).cmxa)
LIBCMA  = $(LIBS:%=%.cma) $(foreach lib,$(MOPSALIBS),$(call lib_file,$(lib)).cma)

## Merlin
MERLIN = $(SRC)/.merlin $(PACKS:%=$(SRC)/%/.merlin)

## C/C++ stubs
C_OBJ  = $(C_SRC:%.c=$(BUILD)/%.o)
CC_OBJ = $(CC_SRC:%.cc=$(BUILD)/%.o)
