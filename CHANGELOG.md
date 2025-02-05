# 1.1

- !241 by @rmonat: pre-release fixes (CHANGELOG.md, ...)
- !265 by @rmonat: bash completion support
- !246 by @aouadjaout: transform bitfields into bitwise operations
- !266 by @rmonat: ci fix
- !144 by @rmonat: leveraging creduce/cvise testcase reduction for Mopsa
- !240 by @rmonat + @aouadjaout: remove `post_to_flow` in stubs
- !253 by @rmonat: state partitioning improvements
- !260 by @antoine_mine: mopsadb: remember object files for target, prints absolute path for objects in libraries
- !250 by @rmonat: fix #206 (panic: pointers do not point to the same type)
- !259 by @aouadjaout: simplify initializers of global and static variables in C
- !261 by @antoine_mine: fix for #211 (Inline assembly parsing messes $ and % symbols)
- !255 by @rmonat: fix #204 (Static packing does not take into account integers within structs)
- !257 by @jboillot: add usage of already implemented abstract transfer functions in congruence domain 
- !249 bv @rmonat: fix #202 (not_found triggered by string length domain )
- !254 by @rmonat: fix #200 (panic catches in interactive engine) + #205 (engine stack overflow)
- !256 by @rmonat: update output of checks to ensure consistency of displayed results
- !248 by @rmonat: fix issue #85 (Not_found raised on ill-behaved program)
- !258 by @aouadjaout: rename effect to change (for OCaml 5.3 compat)
- !180 by @antoine_mine: only abort parsing for fatal Clang errors, not regular errors (nor warnings)
- !251 by @aouadjaout and @rmonat: fix #208 (Bad interaction between state and trace partitioning)
- !252 by @aouadjaout and @rmonat: Cells: remove call to post_to_flow in expand to keep offset constraints in the effects
- !235 by @rmonat: various fixes for lipari tutorial
- !150 by @antoine_mine: translate unknown builtins to a specific AST node instead of aborting
- !247 by @rmonat: CI fixes (mopsa-diff; no manual confirmation of CI job downstream-benchmarks)
- !245 by @aouadjaout: fix the printing of partitioning
- !244 by @antoine_mine: support for Clang 19 as C frontend
- !243 by @antoine_mine: hardcoded no-warning-as-error for incompatible function pointer types
- !242 by @aouadjaout: fix broken CPython CI after merging partitioning
- !130 by @aouadjaout: introduces partitioning in Mopsa
  :warning: Breaking changes, cf. https://gitlab.com/mopsa/mopsa-analyzer/-/merge_requests/130#breaking-changes
- !236 by @Mm72: fix range of sub-expressions of binary operators in universal
- !234 by @edwintorok: fixes in mopsa-build (handling of -Wl,-E)

# 1.0

This is the initial version of Mopsa. See the README.md to know more about Mopsa.

Latest improvements:
- !229 by @rmonat: last pre-release fixes to README, opam file, etc
- !232 by @antoine_mine: update mopsa-build docker image to Ubuntu 24.04 LTS
- !231, !233 by @rmonat: fix header files of multilanguage stubs, required for newer versions of Clang
- !216 by @jboillot: improve the precision of the backward wrap operator for integer intervals
- !221 by @rmonat: display selectivity of all analyses, and count safe pointer checks in C
- !230 by @antoine_mine: asm statements are parsed, and soundness assumptions are raised whenever those statements are encountered during an analysis.
- !218 by @rmonat: improvements to interactive engine (command chaining through `;`, `exit`, documentation)
- !217 by @jboillot: improve symbolic rewriting domain (support of `~`, constant propagation, improve support of `ToBeKSplit`)
- !209 by @antoine_mine: fix for forward/backward modulo operator in congruences
- !228 by @antoine_mine: provide external clang dependencies in mopsa.opam
- !226 by @rmonat: pre-release fixes (removed zlib, fix in release script, improvement of README)
- !225 by @mm72: fix targets check in simplifed produce combiner
- !214 by @rmonat: update minimum requirements
- !220 by @rmonat: fix excluded powerset case when division is not exact
- !215 by @jboillot: correct the semantics of shift-right to match what is done in other domains (division by 2^lhs but rounded towards -oo, rather than rounding to 0 provided by C integer division)
