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

$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml $(BUILD)/%.cmi | $(BUILD)/%.ml.dep $(BUILD)/%.mli.dep
$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.ml $(BUILD)/%.cmi | $(BUILD)/%.ml.dep $(BUILD)/%.mli.dep
$(MLY:$(SRC)/%.mly=$(BUILD)/%.mli): $(BUILD)/%.mli: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mly=$(BUILD)/%.ml.dep): $(BUILD)/%.ml.dep: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mly=$(BUILD)/%.mli.dep): $(BUILD)/%.mli.dep: $(BUILD)/%.mli

.SECONDEXPANSION:
$(PACKS:%=$(BUILD)/%.cmx): $(BUILD)/%.cmx : $$(PACK_DEPS_$$@)
$(PACKS:%=$(BUILD)/%.cmo): $(BUILD)/%.cmo : $$(PACK_DEPS_$$@)

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
	$(QUIET)$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.cmo:
	@mkdir -p $(@D)
	@echo -e "$(CMOMSG)	$(ML_$@)"
	$(QUIET)$(OCAMLFIND) $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.cmi:
	@mkdir -p $(@D)
	@echo -e "$(CMIMSG)	$(MLI_$@)"
	$(QUIET)$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.dep: | $(ML_AUTOGEN) $(MLI_AUTOGEN)
	@mkdir -p $(@D)
	@echo -e "$(DEPMSG)	$(ML_$@)"
	$(QUIET)$(OCAMLFIND) $(OCAMLDEP) $(INCLUDES) -absname $(DEPFLAGS_$@) > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@
