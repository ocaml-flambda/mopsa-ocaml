.. _automated-testcase-reduction:

Automated Testcase Reduction
============================

Testcase reduction is an automated approach aiming at minimizing a test while keeping a specific property. We currently leverage two similar tools performing automated testcase reduction (for C programs only): `creduce <https://github.com/csmith-project/creduce>`_ [PLDI12-Creduce]_ and `cvise <https://github.com/marxin/cvise/>`_. There are currently three possible interactions between automated C reduction and Mopsa, described below. Some additional details can be read in our [CSV24-STTT-preprint]_.


Program reduction for Mopsa's internal errors
_____________________________________________

Whenever an internal error interrupts the analysis of a program, Mopsa automatically yields instructions to trigger the program reduction based on creduce/cvise. This approach relies on a bash script ``tools/reducer-oracle.sh``, for which some environment variables are provided to describe the invoked Mopsa command, the target program, and the error the automated testcase reduction needs to reproduce.


.. warning::

   The error message to search for should not be program specific, as it could hinder the automated testcase reduction. Feel free to edit the error message to remove explicit program expressions or program locations.



In the case of multi-file projects, it will first ask the user to generate a preprocessed file (by calling Mopsa again with ``-c-preprocess-and-exit=file.i``).

Differential-configuration reduction
------------------------------------

We have had some recent successes in differential-configuration reduction, easing the debugging of cases where two different configurations of Mopsa yield contradictory analysis results on a given program. We have typically applied it when an analysis is unsound, and where the culprit (abstract domain or reduction) is included in one configuration and not the other. This reduction can be triggered by bash script ``tools/creduce_compare.sh`` (which includes a usage explanation if no arguments are provided).


Leveraging Mopsa to ease multi-file reduction
---------------------------------------------

One of the current usability barriers to automated test-case reduction through creduce and its sibling cvise is the support of multi-file projects. Indeed, `creduce requires the explicit list of files to be reduced <https://github.com/csmith-project/creduce/blob/31e855e290970cba0286e5032971509c0e7c0a80/creduce/creduce.in#L197>`_ . This list can be difficult to establish on large open-source projects, such as coreutils, where a build system like make takes care of compiling the various sources into an executable, through a list of complex rules. In addition, some large projects may use different files with different compilation options, which would create an additional difficulty in using standard creduce.

Mopsa natively supports the analysis of multi-file C projects. We refer the interested reader to :ref:`the explanation around mopsa-build <mopsa-build>`. Then, ``mopsa-c mopsa.db -make-target=foo`` can be invoked with further option ``-c-preprocess-and-exit=foo.i`` to generate a single, preprocessed C file that will ease the automated testcase reduction pipeline.



.. [PLDI12-Creduce] John Regehr, Yang Chen, Pascal Cuoq, Eric Eide, Chucky Ellison, Xuejun Yang: `Test-case reduction for C compiler bugs <https://users.cs.utah.edu/~regehr/papers/pldi12-preprint.pdf>`_. PLDI 2012: 335-346.


.. [CSV24-STTT-preprint] Raphaël Monat, Abdelraouf Ouadjaout, Antoine Miné: `Easing Maintenance of Academic Static Analyzers. <https://arxiv.org/pdf/2407.12499>`_ To appear in STTT special issue for `Challenges in Software Verification 2024 <https://unive-ssv.github.io/events/2024/06/06/csv.html>`_.
