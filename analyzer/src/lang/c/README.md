Abstract Memory Models
======================

This folder contains abstract memory models for C, with some common utility modules.

* `Machine_numbers` is a border-zone domain translating C numbers (integers and floats) into Universal numbers. It performs checks for overflows and divisions by zero.

* `Pointers` is a non-relational abstract domains for managing pointer bases and offsets.

* `Cells` contains the cell-based abstraction of low-level memory access in C [1].
    
* `Strings` contains specific abstractions for C strings, such as [2].

[1]: "Field-sensitive value analysis of embedded C programs with union types and pointer arithmetics". A. Miné. In LCTES'06, 54–63, 2006.
[2]: "Modular static analysis of string manipulations in C programs". M. Journault, A. Miné, A. Ouadjaout. In SAS'18. LNCS, vol. 11002. 2018.
