#! /bin/bash

# This file is part of MOPSA, a Modular Open Platform for Static Analysis.
#
# Copyright (C) 2024 The MOPSA authors.
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

# Usage: ./dual_mopsa_debugging.sh A B "OPTIONS" will launch a tmux panel split
# into two, with A OPTIONS and B OPTIONS running side by side, and any input
# will be duplicated to both side this is especially useful to compare two
# analyses using the interactive engine

# this can be used to compare the results of different options:
# ./dual_mopsa_debugging.sh "-config=c/cell-itv.json" "-config=c/cell-pack-rel-itv.json" "mopsa-c file.c -engine=interactive"

# it can also be used to compare two different versions of Mopsa 
# ./dual_mopsa_debugging.sh "~/old_mopsa/bin/mopsa-c" "~/new_mopsa/bin/mopsa-c" "file.c -engine=interactive"

# once in the terminal, use [TMUX PREFIX] and type `:setw synchronize-panes` to desynchronize panels. [TMUX PREFIX] is Ctrl-b by default.
# Use [TMUX PREFIX] followeed by o to switch panes.

ORIGINAL=$1
BUGGY=$2
JOINT_OPTIONS=$3

tmux new-session -d -s debug
tmux send-keys -t debug "$ORIGINAL $JOINT_OPTIONS" Enter
tmux split-window -h -t debug
tmux send-keys -t debug "$BUGGY $JOINT_OPTIONS" Enter
tmux set-window-option -t debug:0 synchronize-panes on
tmux attach -t debug
