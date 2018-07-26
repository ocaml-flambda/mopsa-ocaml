# OCaml sources
ML = $(shell find $(SRC) -name "*.ml")
MLI = $(shell find $(SRC) -name "*.mli")
PACKS = $(patsubst $(SRC)/%,%,$(shell find $(SRC)/* -type d))
ML_OF_PACKS = $(PACKS:%=$(BUILD)/%.ml)
TOPML = $(shell $(OCAMLFIND) ocamldep -sort $(SRC)/*.ml)
TOPPACKS = $(patsubst $(SRC)/%,%,$(shell find $(SRC)/* -maxdepth 0 -type d))

# Dependencies
DEPS_ML = $(ML:$(SRC)/%.ml=$(BUILD)/%.dep)
DEPS_MLI = $(MLI:$(SRC)/%.mli=$(BUILD)/%.idep)

# Objects
CMI = $(MLI:$(SRC)/%.mli=$(BUILD)/%.cmi)
CMO = $(filter-out $(CMO_FROM_CMI), $(ML:$(SRC)/%.ml=$(BUILD)/%.cmo))
CMX = $(CMO:%.cmo=%.cmx)

CMO_FROM_CMI = $(CMI:%.cmi=%.cmo)
CMX_FROM_CMI = $(CMI:%.cmi=%.cmx)

CMO_FROM_PACK = $(PACKS:%=$(BUILD)/%.cmo)
CMX_FROM_PACK = $(PACKS:%=$(BUILD)/%.cmx)

TOPCMX = $(TOPML:$(SRC)/%.ml=$(BUILD)/%.cmx) $(TOPPACKS:%=$(BUILD)/%.cmx)

## C/C++ stubs
C_OBJ = $(C_SRC:%.c=$(BUILD)/%.o)
CC_OBJ = $(CC_SRC:%.cc=$(BUILD)/%.o)

## Utility function
include_lineage = \
	$(if $(filter $(1), $(SRC)), \
		-I $(SRC), \
		-I $(1) $(call include_lineage,$(shell realpath --relative-to=. $(1)/..))\
	)

is_directory = $(shell test -d $(basename $(1)) && echo 1 || echo 0)

pack_dir_of_ml = $(shell dirname $(patsubst $(SRC)/%,%,$(1)))

pack_name = $(subst /,.,$(shell $(SED) -e "s/\b\(.\)/\u\1/g" <<< $(1)))
