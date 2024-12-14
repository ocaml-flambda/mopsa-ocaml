.. _c-benchs:

C Benchmarks
============

This page describes how to run Mopsa on two C benchmarks: Juliet Test Suite for C and Coreutils.

Juliet
------

`Juliet Test Suite <https://samate.nist.gov/SRD/testsuite.php>`_ is a large collection of small programs in C, C++, C#, and Java.
Programs are categorized with respect to the `Common Weakness Enumeration <https://cwe.mitre.org>`_ classification.
Each program contains two functions:

- A *good* function that is safe with respect to the target CWE.
- A *bad* function that contains an instance of a bug in the target CWE.

We provide scripts to run Mopsa automatically on the supported part of the Juliet suite in our GitLab `Juliet Benchmark project <https://gitlab.com/mopsa/benchmarks/juliet-benchmarks>`_.
For the moment, Mopsa supports the analysis the C programs from the C/C++ test suite for 12 CWEs:

========= ====================================================
 CWE      Title
========= ====================================================
 CWE121   Stack-based Buffer Overflow
 CWE122   Heap-based Buffer Overflow
 CWE124   Buffer Underwrite
 CWE126   Buffer Over-read
 CWE127   Buffer Under-read
 CWE190   Integer Overflow
 CWE191   Integer Underflow
 CWE369   Divide By Zero
 CWE415   Double Free
 CWE416   Use After Free
 CWE469   Use of Pointer Subtraction to Determine Size
 CWE476   NULL Pointer Dereference
========= ====================================================


The result of each test can be:

- The analysis is *precise* if no alarm is reported in the good function **and** exactly one alarm is reported in the bad function.
- The analysis is *imprecise* if alarms are reported in the good function **or** if more than one alarms are reported in the bad function.
- The analysis is *unsound* if no alarm is reported in the bad function.
- The analysis is *unsupported* if Mopsa doesn't support some C feature used by the program.
- The analysis *fails* if Mopsa crashes.

Preparation
~~~~~~~~~~~

You can use the following analysis scripts to download a copy of Juliet and prepare the analysis:

.. code-block:: shell

   $ git clone https://gitlab.com/mopsa/benchmarks/juliet-benchmark
   $ cd juliet-benchmarks
   $ make prepare

The last command puts Juliet tests in ``src``.

Running All Tests
~~~~~~~~~~~~~~~~~

You can run all the tests with one command:

.. code-block:: shell

   $ make

The results are saved in JSON format in the ``result`` folder.

There are 13261 individual test cases in total, hence the full analysis can take a couple of hours.
If you have several cores, we strongly recommend that you run the tests with:

.. code-block:: shell

   $ make -j<N>

where ``<N>`` is the number of cores.
The analysis of a single program is not parallelized in Mopsa.
However, as the test cases are completely independent, `make` will run their analysis in parallel.

Analyzing a Single CWE
~~~~~~~~~~~~~~~~~~~~~~

It is also possible to run all the tests of a single CWE with:

.. code-block:: shell

   $ make CWE469_Use_of_Pointer_Subtraction_to_Determine_Size

Also, you can select a particular test of a CWE:

.. code-block:: shell

   $ make CWE469_Use_of_Pointer_Subtraction_to_Determine_Size/CWE469_Use_of_Pointer_Subtraction_to_Determine_Size__char_13

Note that tab-completion works: simply type ``make CWE`` and hit tab to get a list of available CWEs and tests.

Analysis Summary
~~~~~~~~~~~~~~~~

After performing some analysis using the commands above, the following command displays a table summarizing the results:

.. code-block:: shell-session

  $ make stats
  +---------+--------------------------------+-----------------+-------------+-------------+-------------+-------------+-------------+-------------+
  |   CWE   |             Title              |      Time       |    Total    |   Success   |  Imprecise  |   Unsound   |   Failure   | Unsupported |
  +---------+--------------------------------+-----------------+-------------+-------------+-------------+-------------+-------------+-------------+
  | CWE121  |  Stack-based Buffer Overflow   |   01:04:51.31   |    2508     |     82%     |     17%     |     0%      |     0%      |     0%      |
  | CWE122  |   Heap-based Buffer Overflow   |   00:40:50.34   |    1556     |     81%     |     18%     |     0%      |     0%      |     0%      |
  | CWE124  |       Buffer Underwrite        |   00:20:14.70   |     758     |     77%     |     22%     |     0%      |     0%      |     0%      |
  | CWE126  |        Buffer Over-read        |   00:16:08.21   |     600     |     88%     |     11%     |     0%      |     0%      |     0%      |
  | CWE127  |       Buffer Under-read        |   00:20:08.91   |     758     |     78%     |     21%     |     0%      |     0%      |     0%      |
  | CWE190  |        Integer Overflow        |   01:30:46.59   |    3420     |     74%     |     25%     |     0%      |     0%      |     0%      |
  | CWE191  |       Integer Underflow        |   01:07:44.16   |    2622     |     78%     |     21%     |     0%      |     0%      |     0%      |
  | CWE369  |         Divide By Zero         |   00:14:16.93   |     497     |     70%     |     29%     |     0%      |     0%      |     0%      |
  | CWE415  |          Double Free           |   00:04:44.59   |     190     |    100%     |     0%      |     0%      |     0%      |     0%      |
  | CWE416  |         Use After Free         |   00:02:57.72   |     118     |    100%     |     0%      |     0%      |     0%      |     0%      |
  | CWE469  | Use of Pointer Subtraction ..  |   00:00:27.08   |     18      |    100%     |     0%      |     0%      |     0%      |     0%      |
  | CWE476  |    NULL Pointer Dereference    |   00:05:18.69   |     216     |    100%     |     0%      |     0%      |     0%      |     0%      |
  +---------+--------------------------------+-----------------+-------------+-------------+-------------+-------------+-------------+-------------+
  |                  Total                   |   05:48:29.23   |    13261    |     79%     |     20%     |     0%      |     0%      |     0%      |
  +------------------------------------------+-----------------+-------------+-------------+-------------+-------------+-------------+-------------+



Coreutils
---------

`GNU Coreutils <https://www.gnu.org/software/coreutils/>`_ is a collection of small command-line utilities.
For the moment, Mopsa can analyze the following 79 programs from Coreutils:

.. code-block:: none

   base32 base64 basename cat chcon chgrp chmod chown chroot cksum comm csplit
   cut dircolors dirname echo env expand false fmt fold getlimits groups head
   hostid id join kill link ln logname md5sum mkdir mkfifo mknod mktemp nice nl
   nohup nproc numfmt od paste pathchk pinky printenv printf pwd readlink
   realpath rmdir runcon seq sha1sum shred shuf sleep split stdbuf stty sync tee
   test timeout touch tr true truncate tsort uname unexpand uniq unlink uptime
   users wc who whoami yes

We provide scripts to run Mopsa on these programs in our `GitLab project <https://gitlab.com/mopsa/benchmarks/coreutils-benchmarks>`_.

We support three scenarios concerning the command-line argument list assumed for the analyzed programs:

- No command-line arguments are passed to ``main``.
- One symbolic argument is passed to ``main`` representing a string with arbitrary content and length.
- A symbolic number of symbolic arguments are passed to ``main``, representing an arbitrary number of arguments of arbitrary content and length (thus corresponding to the most general analysis of the program).

For each scenario, 4 configurations are tested:

=============== =================================================================================================
 Configuration   Description
=============== =================================================================================================
 ``A1``         ``compose(Cells, Intervals)``
 ``A2``         ``compose(product(Cells, StringLength), Intervals)``
 ``A3``         ``compose(product(Cells, StringLength), product(Intervals, Packs(Polyhedra)))``
 ``A4``         ``compose(product(Cells, StringLength, PointerSentinel), product(Intervals, Packs(Polyhedra)))``
=============== =================================================================================================

Consequently, 12 analysis combinations are possible for each one the 79 programs.

Preparation
~~~~~~~~~~~

Firstly, download the benchmarks scripts and prepare the analysis:

.. code-block:: shell

   $ git clone https://gitlab.com/mopsa/benchmarks/coreutils-benchmarks
   $ cd coreutils-benchmarks
   $ make prepare

This will download the sources of Coreutils and put them in ``src`` folder.

Running All Tests
~~~~~~~~~~~~~~~~~

All the tests can be launched by executing:

.. code-block:: shell

   $ make

Similarly to Juliet scripts, it is possible to run tests in parallel with ``make -j<N>``.

Analyzing a Single Program
~~~~~~~~~~~~~~~~~~~~~~~~~~

It is also possible to run Mopsa on particular cases of the benchmarks:

.. code-block:: shell

   $ make nice                     # run all 12 analysis combinations on nice
   $ make nice/one-symbolic-arg    # run all 4 configurations on nice with one symbolic argument
   $ make nice/one-symbolic-arg/a1 # run configuration A1 on nice with one symbolic argument

You can use tab-completion to browse the possible commands.

Analysis Summary
~~~~~~~~~~~~~~~~

After the analysis has finished, a summary of the results can be displayed with the following command:

.. code-block:: shell-session

   $ make stats
