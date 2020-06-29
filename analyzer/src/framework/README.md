`Framework`
==========

The `Framework` module contains the core components of `Mopsa` to perform an analysis.
These components are language-agnostic and are independant of the semantics as well.


`Framework.Ast`
______________

The module `Framework.Ast` allows representing *abstract syntax trees* in `Mopsa`.
The kind of nodes in an AST is not fixed, but can be extended by developers to add new languages easily. 
The modules provided by `Framework.Ast` are:

- `Framework.Ast.Constant`
- `Framework.Ast.Expr`
- `Framework.Ast.Operator`: unary/binary operators
- `Framework.Ast.Program`
- `Framework.Ast.Frontend`: frontends parse source codes into `Mopsa` programs
- `Framework.Ast.Stmt`
- `Framework.Ast.Typ`: types of variables and expression
- `Framework.Ast.Var`
- `Framework.Ast.Visitor`: visitor functions for expressions and statements


`Framework.Core`
----------------

The module `Framework.Core` contains the core data structures used by `Mopsa`:

- `Framework.Core.Alarm`: analysis alarms.
- `Framework.Core.Cases`: implementation of the `cases` monad for performing case-based reasoning.
- `Framework.Core.Context`: flow-insensitive context information.
- `Framework.Core.Eval`: representation of expressions evaluation using the `cases` monad.
- `Framework.Core.Flow`: representation of control flows as continuations.
- `Fremework.Core.Hook`: hooks to observe executed transfer functions.
- `Framework.Core.Id`: generation of unique identifiers for abstract domains.
- `Framework.Core.Lattice`: signature of a lattice.
- `Framework.Core.Log`: for managing logs of executed transfer functions.
- `Framework.Core.Manager`: managers allows domains to access transfer functions of the entire abstraction.
- `Framework.Core.Post`: representation of statements post-states using the `cases` monad.
- `Framework.Core.Query`: mechanism to query domains in order to extract abstract information.
- `Framework.Core.Soundness`: issue soundness warnings about unsupported program features.
- `Framework.Core.Token`: tokens to identify control flows.

`Framework.Sig`
--------------

The module `Framework.Sig` contains the signatures of `Mopsa` abstractions:

- `Framework.Sig.Abstraction.Domain`: standard signature of abstract domains that provides access to all transfer functions.
