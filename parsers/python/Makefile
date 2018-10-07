TARGET_LIBS = mlPythonParser

MOPSAROOT = ../..

# Packs
mlPythonParser = Py_tokens Py_CST Py_parser Py_lexer Py_builtins Py_AST Py_pp Py_CST_to_AST Py_UID

# Packages
PKGS = zarith

# Flags
INCLUDES	:= -I $(MOPSAROOT)/utils/lib
LIBCMXA		:= utils.cmxa
LIBCMA          := $(LIBCMXA:%.cmxa=%.cma)


include $(MOPSAROOT)/make/main.mk
