.. _usage:

Basic Usage
===========

To analyze programs, Mopsa provides five binaries:

- ``mopsa-c`` for C programs,

- ``mopsa-python`` performs a value analysis of Python programs, while ``mopsa-python-types`` performs a type-only analysis,

- ``mopsa-cpython`` for Python programs relying on custom C libraries,

- ``mopsa-universal`` works on a toy imperative language defined within Mopsa.

C analysis
----------

Consider the following program :download:`hello.c <../resources/code/hello.c>`:

.. literalinclude:: ../resources/code/hello.c
   :language: c
   :linenos:

By running ``mopsa-c hello.c`` we get the following report:

.. raw:: html
   :file: ../resources/output/hello.c.out.html

As expected, Mopsa raises an alarm at line 5, corresponding to the invalid access to array ``a`` when index ``i`` reaches ``LEN``.

Mopsa indicates the total number of checks of each kind it performed.
Here, in addition to the failed memory access check, there is an integer overflow check that passed.
It corresponds to the incrementation ``i++``.
By default, Mopsa reports the detailed list of unsuccessful checks but only the number of successful checks.
Using the ``-show-safe-checks`` option, Mopsa will also list the successful checks.
More details on checks are available in the :ref:`reports section <reports>`.

Changing the ``<=`` at line 4 into ``<`` (:download:`hello-fixed.c <../resources/code/hello-fixed.c>`) and running again the analysis, Mopsa reports no alarm.
This proves that the program is free from invalid memory accesses and integer overflows:

.. raw:: html
   :file: ../resources/output/hello-fixed.c.out.html

Analyzing a program containing multiple files is performed similarly, by providing the list of all source files to ``mopsa-c``.
The C front end in Mopsa understands the ``-I`` option to specify preprocessor include directories, and arbitrary C preprocessor and compiler options can be passed with the ``-ccopt`` option (for instance ``-ccopt -DSYMBOL=VALUE``).
By default, Mopsa starts the analysis with the ``main`` C function, and fails if it is not defined.
This can be overridden with the ``-c-entry`` option

You can jump to the :ref:`C benchmarks <c-benchs>` section for more interesting analysis examples in C.



Python analysis
---------------

Python programs are also analyzed by simply calling ``mopsa-python`` on the list of source files.
Consider the following example :download:`hello.py <../resources/code/hello.py>`:

.. literalinclude:: ../resources/code/hello.py
   :language: python
   :linenos:

The missing  ``return`` statement at line 3 makes the function ``absolute`` return ``None``, which triggers a ``TypeError`` in the second call as indicated by the output of Mopsa:

.. raw:: html
   :file: ../resources/output/hello.py.out.html

You can jump to the :ref:`Python benchmarks <py-benchs>` section for more interesting analysis examples in Python.


Multilanguage analysis (Python+C)
---------------------------------

Analyzing multilanguage programs is a bit more involved, as we need to rely on the :ref:`mopsa-build tool <mopsa-build>`.
We illustrate this approach on the running example of our [SAS21]_ paper.
You will need the files :download:`counter.c <../resources/code/counter.c>`, :download:`setup.py <../resources/code/setup.py>` and :download:`count.py<../resources/code/count.py>`.


The first thing to do is build the ``counter.c`` library.
We call the build process with custom wrappers, so that Mopsa stores the relationship between the ``.so`` file and the sources, using  ``mopsa-build python3 setup.py build --force --inplace`` (this first step may require having the packages ``python3-dev python3-setuptools`` being installed on your computer).

Once this is done, we can run the actual analysis of ``count.py`` shown below, using ``mopsa-cpython count.py``.
Note that ``mopsa-cpython`` relies the header files of Python 3.8, which are assumed to be in ``/usr/include/python3.8`` (the path can be changed in ``bin/mopsa-cpython``).

.. literalinclude:: ../resources/code/count.py
   :language: python
   :linenos:

The result is shown below. Mopsa detects both the ``OverflowError`` that may be raised and a silent integer overflow that may also happen. The last error is a false alarm that should be easily avoided once a partitioning domain is used in Mopsa.

.. raw:: html
   :file: ../resources/output/counter.py.out.html

:ref:`Multilanguage benchmarks <cpy-benchs>` refers to the multilanguage benchmarks we used in our experimental evaluation. They can also be run using the SAS 2021 artefact [SAS21-artefact]_.

.. [SAS21] Raphaël Monat, Abdelraouf Ouadjaout, Antoine Miné: `Static Type Analysis by Abstract Interpretation of Python Programs. <https://www-apr.lip6.fr/~mine/publi/article-monat-al-sas21.pdf>`_ SAS 2021: 323-345

.. [SAS21-artefact] Raphaël Monat, Abdelraouf Ouadjaout, Antoine Miné: `Static Type Analysis by Abstract Interpretation of Python Programs -- Artefact. <https://zenodo.org/record/5141314>`_ Zenodo
