AC_INIT([mopsa], [1.0])

AC_CONFIG_FILES([
 Makefile
 utils/Makefile
 parsers/universal/Makefile
 parsers/c/Makefile
 parsers/c_stubs/Makefile
 parsers/python/Makefile
 analyzer/Makefile
])


AC_ARG_ENABLE(
  [c],
  [AS_HELP_STRING([--disable-c], [Turn off C analysis])],
  [AC_SUBST(disable_c,yes)], [])




AC_ARG_ENABLE(
  [python],
  [AS_HELP_STRING([--disable-python], [Turn off Python analysis])],
  [AC_SUBST(disable_python,yes)], [])


AC_OUTPUT
