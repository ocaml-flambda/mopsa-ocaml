TARGET_LIBS = mlCStubsParser

# Packs
mlCStubsParser = c_stubs_parser
c_stubs_parser = range cst parser lexer ast passes cst_to_ast

# Packages
PKGS = zarith

# MOPSA
MOPSAROOT = ../..
MOPSALIBS := utils:utils parsers/c:mlClangParser


include $(MOPSAROOT)/make/main.mk
