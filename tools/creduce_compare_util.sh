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

# /!\ variables FORCE_ASSERT_UNREACH FILE CONF1 REGEX1 CONF2 REGEX2 need to be exported!

clang -c $FILE &> /dev/null &&
    ( ( [ $FORCE_ASSERT_UNREACH -eq 0 ] && grep -q "_mopsa_assert_unreachable();" $FILE) || true )  &&
    ( mopsa-c -no-color -config=c/$CONF1 $FILE 2>&1 > stdout1 || true ) &&
    grep -q "$REGEX1" stdout1 &&
    ( mopsa-c -no-color -config=c/$CONF2 $FILE 2>&1 > stdout2 || true ) &&
    grep -q "$REGEX2" stdout2
