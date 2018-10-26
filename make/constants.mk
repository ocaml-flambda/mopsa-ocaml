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

END=\033[0m
GREEN=\033[32m
LIGHT_GREEN=\033[92m
YELLOW=\033[33m
LIGHT_YELLOW=\033[93m
BLUE=\033[34m
LIGHT_BLUE=\033[94m
MAGENTA=\033[35m
LIGHT_GRAY=\033[37m
GRAY=\033[90m
CYAN=\033[36m
LIGHT_CYAN=\033[96m
DEFAULT=\033[39m

GENMSG=$(GRAY)[GEN]$(END)
DEPMSG=$(LIGHT_GRAY)[DEP]$(END)
MLLMSG=$(GREEN)[MLL]$(END)
MLYMSG=$(LIGHT_GREEN)[MLY]$(END)
CMIMSG=$(YELLOW)[CMI]$(END)
CMOMSG=$(BLUE)[CMO]$(END)
CMXMSG=$(MAGENTA)[CMX]$(END)
CCMSG=$(LIGHT_CYAN)[CC]$(END)
CXXMSG=$(LIGHT_BLUE)[CXX]$(END)
LDMSG=$(DEFAULT)[LD]$(END)
