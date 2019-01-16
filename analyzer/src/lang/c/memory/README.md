Abstract Memory Models
======================

This folder contains abstract memory models for C, with some common utility modules.

* `Machine_numbers` is a border-zone domain translating C numbers (integers and floats) into Universal numbers. It performs checks for overflows and divisions by zero.

* `Pointers` is a non-relational abstract domains for managing pointer bases and offsets.

* `Cells` module contains the cell-based abstraction of low-level memory access in C [1].

	* `Cells.Cell` defines the `cell` type and its related functions.
	
	* `Cells.Expand` is a expension abstraction of cells.
	
	* `Cells.Cell2Scalar` translates cell statements and expressions into C scalar ones.

[1]: "Field-sensitive value analysis of embedded C programs with union types and pointer arithmetics". Antoine Miné. In LCTES'06, 54–63, 2006. ACM.
