.. _mopsa-diff:

``mopsa-diff``
==============

The script ``mopsa-diff`` compares two JSON reports and shows the list of differences, in terms of alarms and analysis time.
It is also possible to compare two folders containing several JSON reports, which is useful when comparing two instances of a benchmark over several programs or configurations.
By default, the script has a return code of 0 if the two logs are equivalent (same set of alarms and same time up to a tolerance), and 1 if a difference is detected (one log has an alarm this is not present in the other log or the time difference exceeds the tolerance threshold).

The script ``mopsa-diff`` accepts the following options:

.. program:: mopsa-diff

.. option:: --summary

   Print a summary of the differences.

.. option:: --ignore-time

   Ignore differences in analysis time (default: ``false``).

.. option:: --time-tolerance <delta>

   Set the tolerated time difference, in milliseconds (default: ``1000``).

.. option:: --regression

   The return code is 1 only if the second log contains an alarm that is not in the first log, or if it takes longer.
   If the second log has no extra alarm and if takes as much or less time, 0 is returned instead.



Example
-------

Consider the programs :download:`hello.c <../resources/code/hello.c>` and :download:`hello-fixed.c <../resources/code/hello-fixed.c>` from the :ref:`Basic Usage section <usage>`.
We analyze both programs, taking care to output to JSON files (using the ``-format=json`` and the ``-output`` options), and compute the difference:

.. code-block:: shell-session

   $ mopsa-c hello.c -format=json -output=hello.json
   $ mopsa-c hello-fixed.c -format=json -output=hello-fixed.json
   $ mopsa-diff hello.json hello-fixed.json

We get the following output:

.. code-block:: shell-session

   1 report compared
   --- hello.json
   +++ hello-fixed.json

   - hello.c:5.4-8: alarm: Invalid memory access
