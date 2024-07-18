#! /bin/bash

# Usage: ./dual_mopsa_debugging.sh A B "OPTIONS" will launch a tmux panel split
# into two, with A OPTIONS and B OPTIONS running side by side, and any input
# will be duplicated to both side this is especially useful to compare two
# analyses using the interactive engine

# this can be used to compare the results of different options:
# ./dual_mopsa_debugging.sh "-config=c/cell-itv.json" "-config=c/cell-pack-rel-itv.json" "mopsa-c file.c -engine=interactive"

# it can also be used to compare two different versions of Mopsa 
# ./dual_mopsa_debugging.sh "~/old_mopsa/bin/mopsa-c" "~/new_mopsa/bin/mopsa-c" "file.c -engine=interactive"

ORIGINAL=$1
BUGGY=$2
JOINT_OPTIONS=$3

tmux new-session -d -s debug
tmux send-keys -t debug "$ORIGINAL $JOINT_OPTIONS" Enter
tmux split-window -h -t debug
tmux send-keys -t debug "$BUGGY $JOINT_OPTIONS" Enter
tmux set-window-option -t debug:0 synchronize-panes on
tmux attach -t debug
