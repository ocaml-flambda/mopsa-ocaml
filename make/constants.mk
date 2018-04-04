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

BSRC = $(BUILD)/$(SRC)
BTESTS = $(BUILD)/$(TESTS)

#PKGS += unix str
INCLUDES += -I $(BUILD) -I $(BSRC)

OCAMLFLAGS :=  -bin-annot $(INCLUDES) $(OCAMLFLAGS) -g
CFLAGS := $(CFLAGS) $(INCLUDES)
CXXFLAGS := $(CXXFLAGS) $(INCLUDES)
LDFLAGS := $(LDFLAGS) -L$(BUILD) -L$(BSRC)
