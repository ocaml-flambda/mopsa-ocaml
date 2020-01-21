dnl
dnl  This file is part of MOPSA, a Modular Open Platform for Static Analysis.
dnl
dnl  Copyright (C) 2017-2019 The MOPSA Project.
dnl
dnl  This program is free software: you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation, either version 3 of the License, or
dnl  (at your option) any later version.
dnl
dnl  This program is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU Lesser General Public License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl


AC_INIT([mopsa], [1.0])


AC_CONFIG_FILES([
 Makefile
 make/constants.mk
 utils/Makefile
 parsers/universal/Makefile
 parsers/c/Makefile
 parsers/c_stubs/Makefile
 parsers/python/Makefile
 analyzer/Makefile
])


dnl Checking for C/C++ compilers
AC_PROG_CC
AC_PROG_CXX



dnl Checking for the OCaml toolchain (ocamlc, ocamlopt, ocamlfind, ...)
AC_CHECK_TOOL(OCAMLC,ocamlc,no)
if test "x$OCAMLC" = "xno"; then
  AC_MSG_ERROR([OCaml compiler not found])
fi


AC_MSG_CHECKING([for ocamlc version])
OCAML_VERSION=`$OCAMLC -version`
MIN_OCAML_VERSION="4.07.0"
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


dnl OCaml libraries
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


dnl Checking for llvm
AC_CHECK_TOOL(LLVMCONFIG,llvm-config)
if test "x$LLVMCONFIG" = "x"; then
  LLVMCONFIG = `ls /usr/bin/llvm-config* | sort -V -r | head -n 1`
  if test "x$LLVMCONFIG" = "x"; then
    AC_MSG_WARN([llvm-config not found. C analyzer will not be compiled.])
  fi
  AC_SUBST([LLVMCONFIG])
fi

if test "x$LLVMCONFIG" != "x"; then
  AC_MSG_CHECKING([for LLVM version])
  LLVM_VERSION=`$LLVMCONFIG --version`
  MIN_LLVM_VERSION="5.0"
  AC_MSG_RESULT([$LLVM_VERSION])
  V=`printf "$LLVM_VERSION\n$MIN_LLVM_VERSION\n" | sort -V | head -n 1`

  if test "$V" != "$MIN_LLVM_VERSION"; then
    AC_MSG_WARN([Mopsa requires LLVM version >= $MIN_LLVM_VERSION. C analyzer will not be compiled.])
    LLVMCONFIG=""
    AC_SUBST([LLVMCONFIG])
  fi

fi


dnl Checking for clang
AC_CHECK_TOOL(CLANG,clang)
if test "x$CLANG" = "x"; then
  CLANG = `ls /usr/bin/clang* | sort -V -r | head -n 1`
  if test "x$CLANG" = "x"; then
    AC_MSG_WARN([clang not found. C analyzer will not be compiled.])
  fi
  AC_SUBST([CLANG])
fi




dnl Deactivation of C analysis

AC_ARG_ENABLE(
  [c],
  [AS_HELP_STRING([--disable-c], [Turn off C analysis])],
  [AC_SUBST(disable_c,yes)], [])

if test "x$LLVMCONFIG" = "x"; then
 AC_SUBST(disable_c,yes)
fi


dnl Deactivation of Python analysis
AC_ARG_ENABLE(
  [python],
  [AS_HELP_STRING([--disable-python], [Turn off Python analysis])],
  [AC_SUBST(disable_python,yes)], [])


dnl Utils
AC_PROG_SED



AC_OUTPUT
