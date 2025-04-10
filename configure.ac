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


AC_INIT([mopsa], [1.0])


AC_CONFIG_FILES([
 parsers/c/lib/parser/dune
 utils/core/version.ml
 META
])

# Version
AC_MSG_CHECKING([for Mopsa's version])
VERSION=`cat VERSION`
AC_MSG_RESULT($VERSION)
AC_SUBST(version,$VERSION)

# Git revision
AC_MSG_CHECKING([for Mopsa's development version])
if test -d .git; then
   # has .git, assuming a development version
   GIT_COMMIT=`git --no-pager log -1 --format=%h`
   GIT_DATE=`git --no-pager log -1 --format=%ai`
   GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
   DEV_VERSION="git branch:$GIT_BRANCH commit:$GIT_COMMIT commit-date:$GIT_DATE"
else
   # no .git, assuming a release version from a tarball
   DEV_VERSION="release"
fi
AC_MSG_RESULT($DEV_VERSION)
AC_SUBST(dev_version,$DEV_VERSION)


# Checking for C/C++ compilers
AC_PROG_CC
AC_PROG_CXX


# Some utility programs
AC_PROG_SED


# Minimal versions
MIN_OCAML_VERSION="4.08.0"
MIN_LLVM_VERSION="6.0"


# Checking for the OCaml toolchain (ocamlc, ocamlopt, ocamlfind, ...)
AC_CHECK_TOOL(OCAMLC,ocamlc,no)
if test "x$OCAMLC" = "xno"; then
  AC_MSG_ERROR([OCaml compiler not found])
fi


AC_MSG_CHECKING([for ocamlc version])
OCAML_VERSION=`$OCAMLC -version`
AC_MSG_RESULT([$OCAML_VERSION])
V=`printf "$OCAML_VERSION\n$MIN_OCAML_VERSION\n" | sort -V | head -n 1`
if test "$V" != "$MIN_OCAML_VERSION"; then
  AC_MSG_ERROR([Mopsa requires ocamlc version >= $MIN_OCAML_VERSION])
fi

AC_CHECK_TOOL(OCAMLFIND,ocamlfind,no)
if test "x$OCAMLFIND" = "xno"; then
  AC_MSG_ERROR([ocamlfind not found])
fi

AC_CHECK_TOOL(OCAMLOPT,ocamlopt,no)
if test "x$OCAMLOPT" = "xno"; then
  AC_MSG_ERROR([ocamlopt compiler not found])
fi

AC_CHECK_TOOL(OCAMLDEP,ocamldep,no)
if test "x$OCAMLDEP" = "xno"; then
  AC_MSG_ERROR([ocamldep not found])
fi

AC_CHECK_TOOL(OCAMLMKLIB,ocamlmklib,no)
if test "x$OCAMLMKLIB" = "xno"; then
  AC_MSG_ERROR([ocamlmklib not found])
fi

AC_CHECK_TOOL(OCAMLLEX,ocamllex,no)
if test "x$OCAMLLEX" = "xno"; then
  AC_MSG_ERROR([ocamllex not found])
fi

AC_CHECK_TOOL(MENHIR,menhir,no)
if test "x$MENHIR" = "xno"; then
  AC_MSG_ERROR([menhir not found])
fi


# OCaml libraries
AC_MSG_CHECKING([for ocaml yojson library])
if $OCAMLFIND query yojson >/dev/null 2>/dev/null; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([not found])
  AC_MSG_ERROR([OCaml yojson library not found])]
fi

AC_MSG_CHECKING([for ocaml zarith library])
if $OCAMLFIND query zarith >/dev/null 2>/dev/null; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([not found])
  AC_MSG_ERROR([OCaml zarith library not found])]
fi
 
AC_MSG_CHECKING([for ocaml apron library])
if $OCAMLFIND query apron >/dev/null 2>/dev/null; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([not found])
  AC_MSG_ERROR([OCaml apron library not found])]
fi


# Checking for llvm
LLVMCONFIG_EXTRA=""
AC_CHECK_TOOL(LLVMCONFIG,llvm-config)
if test "x$LLVMCONFIG" = "x"; then
  # If llvm-config is not found, try to search for llvm-config-XXX
  AC_MSG_CHECKING([for llvm-config-X])
  # FIXME: we are searching only in /usr/bin and /usr/local/bin. We should generalize to $PATH
  LLVMCONFIG=`find /usr/bin /usr/local/bin -name 'llvm-config*' -exec basename {} \; | sort -V -r | head -n 1`
  if test "x$LLVMCONFIG" = "x"; then
    AC_MSG_RESULT([no])
    AC_MSG_WARN([llvm-config not found. C analyzer will not be compiled.])
  else
    AC_MSG_RESULT([$LLVMCONFIG])
    AC_SUBST([LLVMCONFIG])
    LLVMCONFIG_EXTRA=`printf $LLVMCONFIG | $SED "s/llvm-config//g"`
  fi
fi

# Check llvm version
if test "x$LLVMCONFIG" != "x"; then
  AC_MSG_CHECKING([for LLVM version])

  LLVM_VERSION=`$LLVMCONFIG --version`
  AC_MSG_RESULT([$LLVM_VERSION])

  V=`printf "$LLVM_VERSION\n$MIN_LLVM_VERSION\n" | sort -V | head -n 1`
  if test "$V" != "$MIN_LLVM_VERSION"; then
    AC_MSG_WARN([Mopsa requires LLVM version >= $MIN_LLVM_VERSION. C analyzer will not be compiled.])
    LLVMCONFIG=""
    AC_SUBST([LLVMCONFIG])
  else
    AC_CHECK_TOOL(CLANG,clang$LLVMCONFIG_EXTRA)

    LLVM_MAJOR_VERSION=`$LLVMCONFIG --version | cut -f 1 -d .`
    AC_SUBST([LLVM_MAJOR_VERSION])

    if test $LLVM_MAJOR_VERSION -ge 10; then
       LLVM_CCLIBS="-lclang-cpp"
    else
       LLVM_CCLIBS=" -lclangFrontend -lclangSerialization -lclangDriver -lclangTooling -lclangParse -lclangSema  -lclangAnalysis -lclangEdit -lclangAST -lclangLex -lclangBasic"
    fi
    AC_SUBST([LLVM_CCLIBS])
  fi
fi

if test "x$CLANG" = "x"; then
  AC_CHECK_TOOL(CLANG,clang)
fi

if test "x$CLANG" = "x"; then
  LLVMCONFIG=""
  AC_MSG_WARN([clang not found. C analyzer will not be compiled.])
else
  CLANGDIR=$($CLANG -print-resource-dir)
fi 
AC_SUBST(CLANGRESOURCEDIR, "(echo \"$CLANGDIR\")")

# Deactivation of C analysis
AC_ARG_ENABLE(
  [c],
  [AS_HELP_STRING([--disable-c], [Turn off C analysis])],
  [DISABLE_C=yes], [])

# Deactive C if llvm-config was not found
if test "x$LLVMCONFIG" = "x"; then
 DISABLE_C=yes
fi

if test "x$DISABLE_C" != "x"; then
 AC_SUBST(disable_c,yes)
fi



# Deactivation of Python analysis
AC_ARG_ENABLE(
  [python],
  [AS_HELP_STRING([--disable-python], [Turn off Python analysis])],
  [DISABLE_PYTHON=yes], [])

if test "x$DISABLE_PYTHON" != "x"; then
 AC_SUBST(disable_python,yes)
fi


# Activation of byte compilation
AC_ARG_ENABLE(
  [byte],
  [AS_HELP_STRING([--enable-byte], [Turn on compilation of byte binaries])],
  [AC_SUBST(enable_byte,yes)], [])


# OCamlfind's META

LIBS_TO_INSTALL="utils parsers/universal analyzer"
META_ANALYZER_REQUIRES="mopsa.utils mopsa.universal_parser"

if test "x$DISABLE_C" = "x"; then
  LIBS_TO_INSTALL="$LIBS_TO_INSTALL parsers/c parsers/c_stubs"
  META_ANALYZER_REQUIRES="$META_ANALYZER_REQUIRES mopsa.clang_parser mopsa.cstub_parser"
fi

if test "x$DISABLE_PYTHON" = "x"; then
  LIBS_TO_INSTALL="$LIBS_TO_INSTALL parsers/python"
  META_ANALYZER_REQUIRES="$META_ANALYZER_REQUIRES mopsa.python_parser"
fi

AC_SUBST(meta_analyzer_requires,$META_ANALYZER_REQUIRES)
AC_SUBST(libs_to_install,$LIBS_TO_INSTALL)

AC_OUTPUT
