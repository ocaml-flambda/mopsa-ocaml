##################
## Dependencies ##
##################

$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmi): $(BUILD)/%.cmi: $(SRC)/%.mli
$(MLI:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%

$(ML:$(SRC)/%.ml=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(SRC)/%.ml
$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(SRC)/%.cmi
$(ML:$(SRC)/%.ml=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(SRC)/%.ml
$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(SRC)/%.cmi
$(ML:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%

$(MLL:$(SRC)/%.mll=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mll=$(BUILD)/%.dep): $(BUILD)/%.dep: $(BUILD)/%.ml

$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mly=$(BUILD)/%.ydep): $(BUILD)/%.ydep: $(SRC)/%.mly

######################
## OCamlLex recipes ##
######################


$(MLL:$(SRC)/%.mll=$(BUILD)/%.ml):
	@mkdir -p $(@D)
	@echo "[MLL] $<"
	$(OCAMLLEX) -q $< -o $@


####################
## Menhir recipes ##
####################

$(MLY:$(SRC)/%.mly=$(BUILD)/%.ml) $(MLY:$(SRC)/%.mly=$(BUILD)/%.mli):
	@mkdir -p $(@D)
	@echo "[MLY] $<"
	$(MENHIR)  --explain  $< --base `dirname $@`/`basename $@ .ml`


###################
## Ocaml recipes ##
###################

%.cmx:
	@mkdir -p $(@D)
	@echo "[CMX] $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

%.cmo:
	@mkdir -p $(@D)
	@echo "[CMO] $<"
	@$(OCAMLFIND) $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

%.cmi:
	@mkdir -p $(@D)
	@echo "[CMI] $<"
	@$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

%.dep:
	@mkdir -p $(@D)
	@echo "[DEP] $<"
	@$(OCAMLFIND) $(OCAMLDEP) $(INCLUDE_FLAG_$<) $(INCLUDE_FLAG_$<:$(BUILD)%=$(SRC)%) $(INCLUDES) -absname  $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@

%.ydep:
	@mkdir -p $(@D)
	@echo "[DEP] $<"
	@$(MENHIR) --raw-depend --ocamldep '$(OCAMLFIND) $(OCAMLDEP) $(INCLUDES) -I $(SRC)' $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@
