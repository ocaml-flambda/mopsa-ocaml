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

##################
## Dependencies ##
##################

$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmi): $(BUILD)/%.cmi: $(SRC)/%.mli | $(BUILD)/%.mli.dep
$(MLI:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%

$(ML:$(SRC)/%.ml=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(SRC)/%.ml | $(BUILD)/%.ml.dep
$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.cmi
$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.cmi
$(ML:$(SRC)/%.ml=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(SRC)/%.ml | $(BUILD)/%.ml.dep
$(ML:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%

$(MLL:$(SRC)/%.mll=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml | $(BUILD)/%.ml.dep
$(MLL:$(SRC)/%.mll=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.ml | $(BUILD)/%.ml.dep
$(MLL:$(SRC)/%.mll=$(BUILD)/%.ml.dep): $(BUILD)/%.ml.dep: $(BUILD)/%.ml

$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml $(BUILD)/%.cmi | $(BUILD)/%.ml.dep
$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.ml $(BUILD)/%.cmi | $(BUILD)/%.ml.dep
$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmi): $(BUILD)/%.cmi: $(BUILD)/%.mli | $(BUILD)/%.mli.dep
$(MLY:$(SRC)/%.mly=$(BUILD)/%.mli): $(BUILD)/%.mli: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mly=$(BUILD)/%.ml.dep): $(BUILD)/%.ml.dep: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mly=$(BUILD)/%.mli.dep): $(BUILD)/%.mli.dep: $(BUILD)/%.mli

.SECONDEXPANSION:
$(PACKS:%=$(BUILD)/%.cmx): $(BUILD)/%.cmx : $$(PACK_DEPS_$$@) $$(PACK_DIR_$$@)
$(PACKS:%=$(BUILD)/%.cmo): $(BUILD)/%.cmo : $$(PACK_DEPS_$$@) $$(PACK_DIR_$$@)

$(PACKS:%=$(BUILD)/%.ml): $(BUILD)/%.ml : $(SRC)/%
	@mkdir -p $(@D)
	$(QUIET)touch $@


######################
## OCamlLex recipes ##
######################


$(MLL:$(SRC)/%.mll=$(BUILD)/%.ml): $(BUILD)/%.ml: $(SRC)/%.mll
	@mkdir -p $(@D)
	@echo -e "$(MLLMSG)	$^"
	$(QUIET)$(OCAMLLEX) -q $< -o $@


####################
## Menhir recipes ##
####################

$(MLY:$(SRC)/%.mly=$(BUILD)/%.ml): $(BUILD)/%.ml: $(SRC)/%.mly
	@mkdir -p $(@D)
	@echo -e "$(MLYMSG)	$^"
	$(QUIET)$(MENHIR)  --explain  $< --base `dirname $@`/`basename $@ .ml`



###################
## Ocaml recipes ##
###################

%.cmx:
	@mkdir -p $(@D)
	@echo -e "$(CMXMSG)	$(ML_$@)"
	$(QUIET)$(OCAMLFIND) ocamlopt -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.cmo:
	@mkdir -p $(@D)
	@echo -e "$(CMOMSG)	$(ML_$@)"
	$(QUIET)$(OCAMLFIND) ocamlc -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.cmi:
	@mkdir -p $(@D)
	@echo -e "$(CMIMSG)	$(MLI_$@)"
	$(QUIET)$(OCAMLFIND) ocamlc -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.dep: | $(ML_AUTOGEN) $(MLI_AUTOGEN)
	@mkdir -p $(@D)
	@echo -e "$(DEPMSG)	$(ML_$@)"
	$(QUIET)$(OCAMLFIND) ocamldep $(INCLUDES) -absname $(DEPFLAGS_$@) > $@
	$(QUIET)$(SED) -i 's/\bsrc\b/_build/g' $@
