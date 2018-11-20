TARGET_LIBS = mlCStubsParser

# Packs
mlCStubsParser = c_stubs_parser
c_stubs_parser = cst parser lexer ast passes main
c_stubs_parser.passes = predicate_inlining implicit_cast scoping typer cst_to_ast

# Packages
PKGS = zarith

# MOPSA
MOPSAROOT = ../..
MOPSALIBS := utils:utils parsers/c:mlClangParser


include $(MOPSAROOT)/make/main.mk
