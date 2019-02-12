##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2019 The MOPSA Project.                                #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################

TARGET_CLIBS = mlClangParser
TARGET_NATIVES = wrapper

# Packs
mlClangParser = Clang_AST Clang_utils Clang_dump Clang_parser C_AST C_print C_utils C_simplify Clang_to_C C_parser Build_DB

wrapper = Build_DB Build_wrapper

# C++ files
CC_SRC = Clang_to_ml.cc

#External packages, as given to ocamlfind
PKGS = str unix zarith


# Flags

OCAMLC = ocamlc -g -cc "$(CXX)"
OCAMLOPT = ocamlopt -cc "$(CXX)"

LLVMCONFIG := $(if $(shell which llvm-config), llvm-config, $(lastword $(sort $(wildcard /usr/bin/llvm-config-*))))
CLANG := $(if $(shell which clang), clang, $(lastword $(sort $(wildcard /usr/bin/clang-[0-9]*))))
CLANGRESOURCE := $(shell $(CLANG) -print-resource-dir)

CXXFLAGS := -fPIC  -I $(shell ocamlc -where) $(shell $(LLVMCONFIG) --cxxflags) -Wno-strict-aliasing "-DCLANGRESOURCE=\"$(CLANGRESOURCE)\"" $(CXXFLAGS)

LDFLAGS := $(shell $(LLVMCONFIG) --ldflags) $(LDFLAGS)
CCLIBS := -lclangFrontend -lclangSerialization -lclangDriver -lclangTooling -lclangParse -lclangSema \
  -lclangAnalysis -lclangEdit -lclangAST -lclangLex -lclangBasic -lclang \
  $(shell $(LLVMCONFIG) --libs --system-libs) \
  -lstdc++

# MOPSA
MOPSAROOT = ../..
MOPSALIBS := utils:utils



include $(MOPSAROOT)/make/main.mk
