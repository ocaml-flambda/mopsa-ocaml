# Structure of the Python Part

The `python/` folder of the analysis consists in different files and directories:
- `ast`, `ast_compare`, `pp` and `visitor` define the AST of Python used, as well as comparison operator on this ast, pretty printers and visitors
- the entry point of the Python analysis is `program` (though `frontend` is called before to perform the parsing)
- `desugar/` performs dynamic rewriting of Python expressions into other expressions (Python or universal ones), letting other domains handle them afterwards.
- the `data_model/` defines the semantics of Python for most operators:
  + arithmetic operators (+, -, ...)
  + comparison (==, is, <=, ...)
  + augmented assignement (+=, ...)
  + attribute accesses (x.attr)
  + calls (obj())
  + subscript (a[b])
- the semantics of the `data_model/` is supplemented by the semantics described in `objects/`. For example, performing an attribute access `x.attr` usually calls `object.\__getattribute__(x, attr)`, which is handled here.
- parts of some libraries are directly defined in OCaml, and found in folder `libs/`
- `flows/exn` describes the handling of exceptions in Python, which is a special feature in the control-flow analysis.
- the type analysis (see `type_analysis.md`) is described in `types/`:
  + `addr_env` is an abstract environment, mapping variables to sets of addresses
  + `nominal_types` handles analysis of some operators such as `isinstance`.
  + `structural_types` keeps the potential attributes of each addresses, and the analysis of low level attribute accesses used by `data_model/attribute` and `objects/py_object` for example.
  + `type_annot` handles type annotations potentially given as stubs in files `share/stubs/python/typedshed/*.pyi`
  + while the other files are some stubs
