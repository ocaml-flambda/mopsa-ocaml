##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2019 The MOPSA Project.                                #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################

lib_src_dir = $(shell $(SED) -e "s@\([^:]*\):.*@$(MOPSAROOT)/\1/src@g" <<< $(1))

lib_dir = $(shell $(SED) -e "s@\([^:]*\):.*@$(MOPSAROOT)/\1/lib@g" <<< $(1))

lib_file = $(shell $(SED) -e "s@\([^:]*\):\(.*\)@$(MOPSAROOT)/\1/lib/\2@g" <<< $(1))

lineage = \
	$(if $(filter $(1), $(SRC)), \
		$(1), \
		$(1) $(call lineage,$(shell realpath --relative-to=. $(1)/..))\
	)

include_lineage = $(foreach p,$(call lineage,$(1)),-I $(p))

is_directory = $(shell test -d $(basename $(1)) && echo 1 || echo 0)

pack_dir_of_ml = $(shell dirname $(patsubst $(SRC)/%,%,$(1)))

pack_name = $(subst /,.,$(shell $(SED) -e "s/\b\(.\)/\u\1/g" <<< $(1)))

merlin_root_path = $(shell $(SED) -e "s/[^\/]\+\//\.\.\//g" -e "s/\/[^\/]\+$$//g" <<< $(1))

reverse = $(if $(1),$(call reverse,$(wordlist 2,$(words $(1)),$(1)))) $(firstword $(1))

merlin_lineage = $(patsubst $(SRC)%,$(call merlin_root_path,$(1))/_build%,$(call reverse,$(call lineage,$(shell dirname $(1)))))
