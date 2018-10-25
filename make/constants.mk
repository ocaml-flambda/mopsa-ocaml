## Globals
SHELL := /bin/bash
CC ?= cc
CXX ?= g++

OCAMLFIND ?= ocamlfind
OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt
OCAMLDEP ?= ocamldep
OCAMLMKLIB ?= ocamlmklib
OCAMLDOC ?= ocamldoc
OCAMLLEX ?= ocamllex
MENHIR ?= menhir
OCPPACK ?= ocp-pack

OBJSUFFIX = o
LIBSUFFIX = a
DLLSUFFIX = so
EXE =

SED = sed

SRC = src
TESTS = tests
BUILD = _build
BIN = bin
DOC = doc
LIB = lib

ifeq ($(VERBOSE),1)
  QUIET =
else
  QUIET = @
endif

#PKGS += unix str
INCLUDES := $(INCLUDES) -I $(BUILD) $(LIBS:%=-I %) $(foreach lib,$(MOPSALIBS),-I $(call lib_src_dir,$(lib)) -I $(call lib_dir,$(lib)))

OCAMLFLAGS :=  -bin-annot -safe-string -absname $(INCLUDES) $(OCAMLFLAGS) -g
CFLAGS := $(CFLAGS) $(INCLUDES)
CXXFLAGS := $(CXXFLAGS) $(INCLUDES)
LDFLAGS := $(LDFLAGS) -L$(BUILD)
