Abstract Memory Models
======================

This folder contains abstract memory models for C, with some common utility modules.

* `Machine_integers` implements the modular arithmetics of machine integers. It performs checks for overflows and divisions by zero. Computations are then translated into mathematical arithmetics, using domains in the zone `Z_u_num`.

* `Pointers` is a non-relational abstract domains for managing pointer bases and offsets.

* `Cells` module contains the cell-based abstraction of low-level memory access in C [1].

	* `Cells.Cell` defines the `cell` type and its related functions.
	
	* `Cells.Expand` is a expension abstraction of cells.
	
	* `Cells.Cell2Scalar` translates cell statements and expressions into C scalar ones.

[1]: ?
