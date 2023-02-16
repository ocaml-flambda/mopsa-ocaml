Memory Abstractions of the C language
=====================================

This folder contains abstract domains for representing memory of C programs. They are organized into three levels:

1. Structured domains implement the original C memory model, including structs, unions and arrays. They have access to the original data structures of the program.

2. Lowlevel domains have a flattened view of the memory. They handle a subset of C involving dereferences of scalar memory blocks.

3. Scalar domains handle the simplest subset of C restricted to scalar values without pointer dereference.
