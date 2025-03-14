.. _python-stubs:

Python Stubs
============

If a library function is not available for analysis (for example, if it is implemented in C, or if the Python source is not available), there are three ways to stub these functions:

#. Using Python type annotations. Note that in this case, the value analysis will lose precision when using those type annotations. Some annotations are provided in the ``.pyi`` files in :mopsa:`share/mopsa/stubs/python/typeshed`. Most of the time, the `typeshed <https://github.com/python/typeshed/tree/master/>`_ project contains annotations for the standard library.

#. By providing a pure-Python implementation of a function. See for example the stubs for the trigonometric function in :mopsa:`share/mopsa/stubs/python/math.py`.

#. By implementing a transfer function into Mopsa itself. See for example the rewriting performed upon a call to the sum operator, defined in :mopsa:`analyzer/src/mopsa_analyzer/lang/python/libs/py_std.ml`.
