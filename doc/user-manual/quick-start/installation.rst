Installation
============

Mopsa is developed under Linux.
It has been known to compile under Ubuntu, Debian, Gentoo.
It also compiles on Windows 10 with WSL2 (Windows Subsystem for Linux), and possibly on other platforms.


Downloading Mopsa
-----------------

The source code of Mopsa can be found at `<https://gitlab.com/mopsa/mopsa-analyzer>`_.
Use Git to get access to the latest version:

.. code-block:: shell-session

   $ git clone https://gitlab.com/mopsa/mopsa-analyzer.git
   $ cd mopsa-analyzer

Installing Mopsa (easy way)
---------------------------

You can use OCaml's package manager (opam), to resolve dependencies and install Mopsa. 
Here are instructions to install opam `<https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system>`_.


.. code-block:: shell-session

   $ LANG=C opam pin add mopsa --with-doc --with-test .


Installing Mopsa from source
----------------------------

Dependencies
++++++++++++

To build Mopsa, first make sure you have the following dependencies:

- `Apron <https://antoinemine.github.io/Apron/doc/>`_
- `autoconf <https://www.gnu.org/software/autoconf/>`_
- `CamlIDL <https://caml.inria.fr/pub/old_caml_site/camlidl/>`_
- `GMP <https://gmplib.org/>`_
- `LLVM and Clang <https://clang.llvm.org/>`_ (version >= 6.x, required for C analysis)
- `GNU make <https://www.gnu.org/software/make/>`_
- `Menhir <http://gallium.inria.fr/~fpottier/menhir>`_
- `MPFR <https://www.mpfr.org/>`_
- `OCaml <https://ocaml.org/>`_ (version >= 4.12.0)
- `opam <https://opam.ocaml.org/>`_ (version >= 2)
- `Yojson <https://github.com/ocaml-community/yojson>`_
- `Zarith <https://github.com/ocaml/Zarith>`_

The OCaml dependencies can be installed with the following command using `opam`:

.. code-block:: shell-session

   $ LANG=C opam install --deps-only --with-doc --with-test .


If you are using Ubuntu 20.04, you can use the following commands to install the dependencies:

.. code-block:: shell-session

   $ sudo apt install opam llvm clang llvm-dev libclang-dev libclang-cpp10-dev libgmp-dev libmpfr-dev autoconf pkg-config
   $ opam init --compiler 4.12.0
   $ eval $(opam env)
   $ LANG=C opam install --deps-only --with-doc --with-test .


For Ubuntu 22.04, you can use:

.. code-block:: shell-session

   $ sudo apt install opam llvm clang llvm-dev libclang-dev libclang-cpp13-dev libgmp-dev libmpfr-dev pkg-config
   $ opam init
   $ eval $(opam env)
   $ LANG=C opam install --deps-only --with-doc --with-test .


Building
++++++++

To compile Mopsa, issue the classic commands:

.. code-block:: shell-session

    $ ./configure
    $ make

The binaries will be available in the ``bin/`` sub-directory.

You can install Mopsa in the active opam switch with:

.. code-block:: shell-session

    $ make install

You can test your installation by running ``mopsa -v``.
If this succeeds, you can jump to :ref:`using Mopsa <usage>`.

.. note::
   When ``configure`` cannot find LLVM/Clang in your system, it outputs the following warning:

   .. code-block:: none

      configure: WARNING: llvm-config not found. C analyzer will not be compiled.

   If you want to use Mopsa for analyzing Python only, you can ignore this warning and continue the build process.
   Otherwise, if you want to analyze C code, make sure to install a correct version of LLVM/Clang.

   If LLVM/Clang is installed in a non-standard location or you want to use a different version, you can set the ``LLVMCONFIG`` environment to the location of the ``llvm-config`` script before calling ``./configure``.
   For instance:

   .. code-block:: shell-session

      $./configure LLVMCONFIG=/usr/bin/llvm-config-10



Advanced Build Options
++++++++++++++++++++++


The ``configure`` script accepts some options:

.. program:: configure

.. option:: --disable-c

   disable the C analysis

.. option:: --disable-python

   disable the Python analysis

.. option:: --enable-byte

   enable the compilation of bytecode binaries (in addition to native code binaries, which are always built)

In addition, several environment variables can be set before calling ``configure`` to alter how Mopsa is built:

.. envvar:: LLVMCONFIG

   full path to the ``llvm-config`` script installed with LLVM/Clang

and the usual ``configure`` variables configuring the C compiler (``CC``, ``CFLAGS``), the C++ compiler (``CXX``, ``CXXFLAGS``), the preprocessor (``CPPFLAGS``), and the linker (``LDFLAGS``, ``LIBS``).

