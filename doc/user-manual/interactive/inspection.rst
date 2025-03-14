Inspection
==========

The following commands are useful to inspect the state of the interpreter:

``print <var>``
  Print the value of a variable.

``env``
  Print the current abstract environment.

``state``
  Print the full abstract state (map from flow tokens to environment)

``state > <file>``
  Save the current abstract environment in ``<file>``.

``backtrace``
  Print the current callstack.

``mopsa_bt``
  Print the callstack of Mopsa

``info alarms``
  Print the list of raised alarms.

``info tokens``
  Print the list of flow tokens.

``info breakpoints``
  Print the list of breakpoints.


