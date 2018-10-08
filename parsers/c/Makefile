TARGET_CLIBS = mlClangParser
TARGET_NATIVES = wrapper

MOPSAROOT = ../..

# Packs
mlClangParser = Clang_AST Clang_utils Clang_dump Clang_parser C_AST C_print C_utils C_simplify Clang_to_C Build_DB

wrapper = Build_DB Build_wrapper

#C++ files
CC_SRC = Clang_to_ml.cc

#External packages, as given to ocamlfind
PKGS = str unix zarith


# Flags

OCAMLC = ocamlc -g -cc "$(CXX)"
OCAMLOPT = ocamlopt -cc "$(CXX)"
INCLUDES   := -I $(MOPSAROOT)/utils/lib

LIBS := utils

LLVMCONFIG := $(if $(shell which llvm-config), llvm-config, $(lastword $(sort $(wildcard /usr/bin/llvm-config-*))))
CLANGRESOURCE := $(shell $(LLVMCONFIG) --libdir)/clang/$(shell $(LLVMCONFIG) --version)

CXXFLAGS := -fPIC  -I $(shell ocamlc -where) $(shell $(LLVMCONFIG) --cxxflags) -Wno-strict-aliasing "-DCLANGRESOURCE=\"$(CLANGRESOURCE)\"" $(CXXFLAGS)

LDFLAGS := $(shell $(LLVMCONFIG) --ldflags) $(LDFLAGS)
CCLIBS := -lclangFrontend -lclangSerialization -lclangDriver -lclangTooling -lclangParse -lclangSema \
  -lclangAnalysis -lclangEdit -lclangAST -lclangLex -lclangBasic -lclang \
  $(shell $(LLVMCONFIG) --libs --system-libs) \
  -lstdc++



include $(MOPSAROOT)/make/main.mk
