AC_INIT([mopsa], [1.0])


AC_CONFIG_MACRO_DIR([m4])

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

AC_PROG_CC

AC_PROG_CXX

AC_PROG_SED

AC_PROG_OCAML
if test "x$OCAMLC" = "xno"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_MSG_CHECKING([OCaml version >= 4.07.1])
AX_COMPARE_VERSION([$OCAMLVERSION],[ge],[4.07.1],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([$OCAMLVERSION])
  AC_MSG_ERROR([Mopsa requires OCaml version >= 4.07.1])]
)

AC_PROG_FINDLIB
if test "x$OCAMLFIND" = "xno"; then
  AC_MSG_ERROR([ocamlfind not found])
fi

AC_CHECK_OCAML_PKG([zarith])
if test "x$OCAML_PKG_zarith" = "xno"; then
  AC_MSG_ERROR([zarith library not found])
fi

AC_CHECK_OCAML_PKG([yojson])
if test "x$OCAML_PKG_yojson" = "xno"; then
  AC_MSG_ERROR([yojson library not found])
fi

AC_PROG_OCAMLLEX
if test "x$OCAMLLEX" = "xno"; then
  AC_MSG_ERROR([ocamllex not found])
fi

AC_CHECK_TOOL(MENHIR,menhir,
  AC_MSG_ERROR([menhir not found])
)


AC_ARG_ENABLE(
  [c],
  [AS_HELP_STRING([--disable-c], [Turn off C analysis])],
  [AC_SUBST(disable_c,yes)], [])




AC_ARG_ENABLE(
  [python],
  [AS_HELP_STRING([--disable-python], [Turn off Python analysis])],
  [AC_SUBST(disable_python,yes)], [])


AC_OUTPUT
