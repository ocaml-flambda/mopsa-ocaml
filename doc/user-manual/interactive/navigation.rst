Navigation
==========

Several navigation commands are provided to follow the execution steps of an analysis.
Most commands are inspired by ``gdb``.

Program
-------

The following commands allow navigating throught the statements of a program:

``next``
  Execute until reaching the next statement in the program. Step over function calls.

``step``
  Execute until reaching the next statement in the program. Step into function calls.

``break <file:line>``
  Add a breakpoint at ``line`` in ``file``.

``break <function>``
  Add a breakpoint at ``function``.

``break #a``
  Will break at the next alarm, and go back to the beginning of the analysis of the statement generating the alarm.

``break @name``
  Add a named breakpoint, the analysis will stop when executing the `S_break name` statement

``backward``
  Go backward to the calling site

``continue``
  Execute until reaching the next breakpoint.

``finish``
  Finish the current function.


Interpreter
-----------

The following commands allow navigating through the interpretation tree:

``nexti``
  Stop at the next interpreter action of the same level. Step over children actions.

``stepi``
  Stop at the next interpreter action.

``where``
  Show the next action of the interpreter.


Replaying interactive sessions
______________________________

The commands below allow you to save & load script files, to ease replaying of interactive sessions.

``set script <file>``
  store the commands passed to the interactive engine into a file

``load script <file>``
  pass commands stored in a file to the interactive engine
