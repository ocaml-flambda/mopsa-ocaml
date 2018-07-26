#################
## Ocaml rules ##
#################

$(CMX): $(BUILD)/%.cmx: $(SRC)/%.ml | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMO): $(BUILD)/%.cmo: $(SRC)/%.ml | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling $<"
	@$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -package "$(PKGS)" $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMI): $(BUILD)/%.cmi: $(SRC)/%.mli   | $(BUILD)/%.idep
	@mkdir -p $(@D)
	@echo "Compiling $<"
	@$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMX_FROM_CMI): $(BUILD)/%.cmx: $(SRC)/%.ml $(BUILD)/%.cmi | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMO_FROM_CMI): $(BUILD)/%.cmo: $(SRC)/%.ml $(BUILD)/%.cmi  | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling $<"
	@$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -package "$(PKGS)" $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

.SECONDEXPANSION:
$(CMX_FROM_PACK): $(BUILD)/%.cmx: $$(PACK_DEP_$$@) | $(BUILD)/%.ml
	@mkdir -p $(@D)
	@echo "Packing $*"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(INCLUDE_FLAG_$@) $(PACK_FLAG_$@) -pack $+ -o $@

$(ML_OF_PACKS): $(BUILD)/%.ml : $(SRC)/%
	@mkdir -p $(@D)
	@touch $@

########################
## Dependencies rules ##
########################

$(DEPS_ML): $(BUILD)/%.dep: $(SRC)/%.ml | $(ML_OF_PACKS)
	@mkdir -p $(@D)
	@echo "Generating dependencies for $<"
	@$(OCAMLFIND) $(OCAMLDEP) -native $(OCAMLINC) $(INCLUDE_FLAG_$<) $(INCLUDE_FLAG_$<:$(BUILD)%=$(SRC)%) $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@


$(DEPS_MLI): $(BUILD)/%.idep: $(SRC)/%.mli | $(ML_OF_PACKS)
	@mkdir -p $(@D)
	@echo "Generating dependencies for $<"
	@$(OCAMLFIND) $(OCAMLDEP) -native $(OCAMLINC) $(INCLUDE_FLAG_$<) $(INCLUDE_FLAG_$<:$(BUILD)%=$(SRC)%) $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@
