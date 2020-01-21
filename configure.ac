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


dnl Checking for the OCaml toolchain
AC_CHECK_TOOL(OCAMLC,ocamlc)

AC_MSG_CHECKING([for OCaml version])
OCAML_VERSION=`$OCAMLC -version`
MIN_OCAML_VERSION="4.07.1"
AC_MSG_RESULT([$OCAML_VERSION])
V=`printf "$OCAML_VERSION\n$MIN_OCAML_VERSION\n" | sort -V | head -n 1`
if test "$V" != "$MIN_OCAML_VERSION"; then
  AC_MSG_ERROR([Mopsa requires OCaml version >= $MIN_OCAML_VERSION])]
fi

AC_CHECK_TOOL(OCAMLFIND,ocamlfind)
AC_CHECK_TOOL(OCAMLOPT,ocamlopt)
AC_CHECK_TOOL(OCAMLDEP,ocamldep)
AC_CHECK_TOOL(OCAMLMKLIB,ocamlmklib)
AC_CHECK_TOOL(OCAMLLEX,ocamllex)
AC_CHECK_TOOL(MENHIR,menhir)


dnl Checking for OCaml libraries
AC_MSG_CHECKING([for zarith library])
if $OCAMLFIND query zarith >/dev/null 2>/dev/null; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([not found])
  AC_MSG_ERROR([Mopsa requires OCaml zarith library])]
fi


AC_MSG_CHECKING([for yojson library])
if $OCAMLFIND query yojson >/dev/null 2>/dev/null; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([not found])
  AC_MSG_ERROR([Mopsa requires OCaml yojson library])]
fi


AC_MSG_CHECKING([for apron library])
if $OCAMLFIND query apron >/dev/null 2>/dev/null; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([not found])
  AC_MSG_ERROR([Mopsa requires OCaml apron library])]
fi


AC_PROG_SED


dnl Deactivation of C analysis
AC_ARG_ENABLE(
  [c],
  [AS_HELP_STRING([--disable-c], [Turn off C analysis])],
  [AC_SUBST(disable_c,yes)], [])



dnl Deactivation of Python analysis
AC_ARG_ENABLE(
  [python],
  [AS_HELP_STRING([--disable-python], [Turn off Python analysis])],
  [AC_SUBST(disable_python,yes)], [])


AC_OUTPUT
