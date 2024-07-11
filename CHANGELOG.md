# 1.0~pre5

- !214 by @rmonat: update minimum requirements
- !220 by @rmonat: fix excluded powerset case when division is not exact
- !215 by @boillot: correct the semantics of shift-right to match what is done in other domains (division by 2^lhs but rounded towards -oo, rather than rounding to 0 provided by C integer division)
- !225 by @mm72: fix targets check in simplifed produce combiner

# 1.0~pre4

Remove clang dependency to simplify install through `opam`.

# 1.0~pre3

Initial version of Mopsa.
