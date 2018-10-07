TARGET_LIBS = mlUniversalParser

MOPSAROOT = ../..

# Packs
mlUniversalParser = U_ast U_ast_printer U_parser U_lexer U_file_parser

# Flags
INCLUDES	:= -I $(MOPSAROOT)/utils/lib
LIBCMXA		:= utils.cmxa
LIBCMA          := $(LIBCMXA:%.cmxa=%.cma)


include $(MOPSAROOT)/make/main.mk
