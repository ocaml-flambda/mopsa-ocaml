#!/usr/bin/env bash

# Temporary usage:
#   Run: source ./demo-completion.sh
#
# Permanent usage:
#   Run: echo "source $(readlink -f .)/demo-completion.sh" >> ~/.bash_completion

_mopsa ()
{
    IFS=$'\n'
    COMPREPLY=($(${COMP_WORDS[0]} --complete "${COMP_WORDS[@]:1:COMP_CWORD}"))
}

complete -o default -F _mopsa mopsa mopsa.exe mopsa-build mopsa-c mopsa-cfg mopsa-configure mopsa-cpython mopsa-db mopsa-make mopsa-python mopsa-python-types mopsa-universal

