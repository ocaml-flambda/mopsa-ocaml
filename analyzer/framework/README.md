`Framework`
==========

The `Framework` module contains the core components of `Mopsa`.
These modules are language-agnostic and provide the necessary infrastructure to develop a static analyis of the targeted language.


Extensible Ast
--------------

The first step to develop a new analysis is to define the syntax of the targted language.
`Mopsa` features an extensible abstract syntax tree that simplifies the definition of a new language.
Here is a summary of the provided modules to manage the extensible AST:


| Module | Description |
|:------:|:-----------:|
| `Framework.Ast.Constant` | Constants |
| `Framework.Ast.Var` | Variables |
| `Framework.Ast.Operator` | Unary/binary operators |
| `Framework.Ast.Expr` | Expressions |
| `Framework.Ast.Typ` | Types of variables and expression
| `Framework.Ast.Stmt` | Statements |
| `Framework.Ast.Program` | Programs |
| `Framework.Ast.Frontend` | Frontends parse source codes into programs |
| `Framework.Ast.Visitor` | Visiting functions for expressions and statements |


Core Definitions
-----------------

The module `Framework.Core` defines the following core data structures used by `Mopsa`:

| Module | Description |
|:------:|:-----------:|
| `Framework.Core.Alarm` | Alarms represent potential bugs of the analyzed program |
| `Framework.Core.Token` | Tokens to identify control flows |
| `Framework.Core.Flow` | Representation of control flows as continuations |
| `Framework.Core.Context` | flow-insensitive context information |
| `Framework.Core.Cases` | Definition of the `cases` monad for performing case-based reasoning |
| `Framework.Core.Eval` | Results of abstract evaluations of expressions, encoded with the `cases` monad |
| `Framework.Core.Post` | Results of abstract execution of statements, encoded with the `cases` monad |
| `Framework.Core.Lattice` | signature of a lattice |
| `Framework.Core.Log` | Logs of executed transfer functions |
| `Framework.Core.Manager` | Managers allow domains to access transfer functions of the toplevel abstraction |
| `Framework.Core.Query` | Mechanism to query domains in order to extract abstract information |
| `Fremework.Core.Hook` | Hooks to observe transfer functions before and after being executed |
| `Framework.Core.Id` | Generation of unique identifiers for abstract domains |
| `Framework.Core.Soundness` | Notify soundness problems (e.g. unsupported program features) |


Signatures
----------

Signatures of abstractions are defined in `Framework.Sig.Abstraction`.
They are organizied as a hierarchy with an increasing level of complexity:

- The simplest signature is `Value` that is useful to define non-relational value abstractions, such as signs and intervals.
  Domains need to define the lattice of the abstract values and how operators affect them.

- The signature `Simplified` is a minmalist interface for defining leaf abstract domains that can interpret statements using
  their local information without interacting with other domains.

- The signature `Domain` is a more general interface that allows domains to interpret statements, evaluate expressions and
  interact with other abstractions through the toplevel manager.

- The signature `Stateless` is a special case of `Domain`.
  It is useful to implement domains that don't have a local state but need to interact with other domains.
  Iterators of compound statements (loops, blocks, etc.) are an example of such domains.

- Finally, the most generic signature is `Stacked`.
  It extends the signature `Domain` by adding the possibility to perform unification during lattice operations.
