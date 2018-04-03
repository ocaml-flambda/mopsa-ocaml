#Get 1-depth sub-directories
subdirs = $(filter-out $(1),\
		$(patsubst $(1)/%,%,\
			$(shell find $(1) -maxdepth 1 -type d)\
		)\
	)

## Packs
PACKS = $(subst /,., \
		$(patsubst $(SRC)/%,%, \
			$(shell find $(SRC)/* -type d) \
		) \
	)
TOP_PACKS = $(call subdirs,$(SRC))


## ML Sources
SML= $(shell find $(SRC) -name "*.ml")
SMLI= $(shell find $(SRC) -name "*.mli")
BML = $(SML:$(SRC)/%=$(BSRC)/%)
BMLI= $(SMLI:$(SRC)/%=$(BSRC)/%)
PACKS_ML = $(foreach pack, $(PACKS),$(subst .,/,$(BSRC)/$(pack)).ml)
ALL_ML = $(BML) $(PACKS_ML) $(BMLI)

## Lex/Yacc sources
SMLL = $(shell find $(SRC) -name "*.mll")
SMLY = $(shell find $(SRC) -name "*.mly")
BMLL = $(SMLL:$(SRC)/%=$(BSRC)/%)
BMLY = $(SMLY:$(SRC)/%=$(BSRC)/%)
BSMLL = $(BMLL:%.mll=%.ml)
BSMLY = $(BMLY:%.mly=%.ml)
BYCMX = $(BMLY:%.mly=%.cmx)


## Top-level ml files
TOP_SML = $(wildcard $(SRC)/*.ml)

# This function gives the list of cmx files needed to build some argument target
top_cmx = $(if $($(1)), $($(1):%=$(BSRC)/%.cmx), $(TOP_SML:%.ml=$(BUILD)/%.cmx) $(TOP_PACKS:%=$(BSRC)/%.cmx))

BTARGET = $(BUILD)/$(TARGET)

## Unit tests
TESTS_ML = $(wildcard $(TESTS)/*.ml)
TESTS_BML = $(TESTS_ML:$(TESTS)/%=$(BTESTS)/%)
TESTS_CMX = $(TESTS_BML:%.ml=%.cmx)
TESTS_EXE = $(TESTS_BML:%.ml=%)
TESTS_DEP = $(TESTS_BML:%.ml=%.dep)

## C/C++ stubs
C_OBJ = $(C_SRC:%.c=$(BSRC)/%.o)
CC_OBJ = $(CC_SRC:%.cc=$(BSRC)/%.o)

# This function generates the include directives recursively towards the build directory
includes = \
	$(if $(filter $(1), $(SRC)), \
		-I $(SRC), \
		-I $(1) $(call includes,$(shell realpath --relative-to=. $(1)/..))\
	)
