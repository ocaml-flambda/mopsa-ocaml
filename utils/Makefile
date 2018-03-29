all: lib-native-c

TARGET = utils
MOPSAROOT = ..

intervals = float intBound floatItv intItv

#C files
C_SRC = intervals/floats_x86_64.c

#External packages, as given to ocamlfind
PKGS = str gmp zarith camlidl

include $(MOPSAROOT)/make/main.mk
