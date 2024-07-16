# 1.0

This is the initial version of Mopsa. See the README.md to know more about Mopsa. 

Latest improvements:
- !229 by @rmonat: last pre-release fixes to README, opam file, etc
- !231 by @rmonat: fix header files of multilanguage stubs, required for newer versions of Clang
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
