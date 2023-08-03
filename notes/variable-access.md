# Variable Access

**Aggregates**
`c.memory.aggregates` takes care of desugaring accesses to arrays and records.
It is also responsible for variable declarations (e.g., `int x;`).
_Importantly_, it forwards them to `c.memory.lowlevel.cells` as `S_add` and not 
as `S_c_declaration` (whose handling seems to be dead code in `c.memory.lowlevel.cells`).

**Cells**
`c.memory.lowlevel.cells` corresponds effectively to C variables + (if enabled) dynamically 
allocated resources. To add a domain that tracks something "for every variable", this 
domain should be the right entry point. We can intercept the creation of variables and 
reads and writes of them. The most important information that we can get out of this domain 
is the list of currently available variables (called "cells"). For an example, see `exec_havoc`.

The list of cells can be obtained with:
```OCaml
let cells_of c = OffCells.fold (fun z c a -> ((Cells.elements c) @ a)) c [] in
let cells = CellSet.fold (fun a b c -> cells_of b @ c ) env.cells []
```


**Pointers**
`c.memory.scalars.pointer` corresponds to the actual abstraction for pointer variables 
(tracked by cells). It tracks their contents and it introduces an auxillary variable 
`offset(p)` that tracks the offset of a pointer into a block. This domain can function 
as a source of inspiration how we can create auxillary variables.

The list of pointers tracked by this domain can be obtained with:
```OCaml
let dom = Map.fold (fun v x c -> v :: c) env []
```

**Machine Integers**
`c.memory.scalars.machine_numbers` performs number checking and then effectively forwards
all operations to the universal domains for intervals, `universal.numeric.values.intervals.float` 
and `universal.numeric.values.intervals.integer`.

**Float and Integer Intervals**
`universal.numeric.values.intervals.float` and `universal.numeric.values.intervals.integer` 
do interval abstraction. They do not track variables themselves and, instead, are lifted to 
finite maps from variable names to elements by `nonrel`.

**Non-Relational Domains**
The operator `nonrel` lifts domains for individual values to an actual Mopsa domain.
Its values are maps that, per variable, store the abstraction for the variable.
The variables can be auxillary variables or program variables. 
This is probably not the right entry point to see which variables are currently around, since 
the domain (in the map sense) could contain arbitrary variables including auxillary variables.

The domain (in the map sense) can be accessed with:
```OCaml
let dom = VarMap.fold (fun v a c -> v :: c) map []
```