# Objectives

The goal of this type analysis is to catch uncaught Python exceptions. A specific emphasis is made on `TypeError`s and `AttributeError`s.

This analysis is described in more details in a paper *TODO*.

# Demo

Let's try to analyze `fspath.py`, which is in `doc/python/` (from the root directory).

From the root directory of MOPSA, you can run the following command: `./bin/mopsa-python-types doc/python/fspath.py`

The analysis should yield something like this:

```
Analysis terminated successfully

doc/python/fspath.py:11.12-68: Uncaught Python exception
  9:             return res
  10:         else:
  11:             raise TypeError("__fspath__ should return str or bytes")
  12:     else:
  13:         raise TypeError("input should have type str, bytes or attribute __fspath__, found type %s instead")
  Cause:
          Uncaught Python exception: TypeError: __fspath__ should return str or bytes
  Call trace:
  	from doc/python/fspath.py:29.9-26: fspath:148
  	from doc/python/fspath.py:32.4-10: main:149

Summary of detected alarms:
  Uncaught Python exception: 1
  Total: 1
Time: 0.011s
```

The analysis found one uncaught exception. Each of those uncaught exceptions are then described in more details.

In our example, the exception is raised at line 11 of the file.
An excerpt of the analyzed code is displayed, as well as more details about the raised exceptions:
- the cause gives the kind of exception raised (a TypeError), and message associated with the exception (here, "__fspath__ should return str or bytes"). This output is similar to the last line of Python's traceback should this exception be raised in a concrete execution.
- the call trace details the nested function calls needed to reach this exception. In our case, the innermost called function was `fspath`, called from line 29. This function was called in the `main` function, itself called by the statement line 32.

## Getting more informations

You can access the last abstract program state of the analysis using the `-lflow` option.

*TODO* description
