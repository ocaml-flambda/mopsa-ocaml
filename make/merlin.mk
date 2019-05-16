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

define generate_merlin_paths =
 ROOT_$(1) = $$(call merlin_root_path,$(1))
 MERLIN_PATHS_$(1) = \
	S $$(ROOT_$(1))/src/** \n\
	$$(foreach p,$$(call merlin_lineage,$(1)),B $$(p)/**\n)\
	$$(foreach p,$$(filter-out -I $$(BUILD),$$(INCLUDES)),B $$(ROOT_$(1))/$$(p)/**\n)
endef


$(foreach m,$(MERLIN),$(eval $(call generate_merlin_paths,$(m))))

merlin: $(MERLIN)

clean-merlin:
	-rm $(MERLIN)

$(MERLIN):
	@echo -e "$(GENMSG)	$@"
	$(shell echo -e "$(MERLIN_PATHS_$@)" >> $@)
	$(file >>$@,PKG $(PKGS))
