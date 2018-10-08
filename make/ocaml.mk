#################
## Ocaml rules ##
#################

$(CMX): $(BUILD)/%.cmx: $(SRC)/%.ml | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmx] $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMO): $(BUILD)/%.cmo: $(SRC)/%.ml | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmo] $<"
	@$(OCAMLFIND) $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMI): $(BUILD)/%.cmi: $(SRC)/%.mli   | $(BUILD)/%.idep
	@mkdir -p $(@D)
	@echo "Compiling [cmi] $<"
	@$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMX_FROM_CMI): $(BUILD)/%.cmx: $(SRC)/%.ml $(BUILD)/%.cmi | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmx] $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@

$(CMO_FROM_CMI): $(BUILD)/%.cmo: $(SRC)/%.ml $(BUILD)/%.cmi  | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmo] $<"
	@$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -package "$(PKGS)" $(INCLUDE_FLAG_$<) $(PACK_FLAG_$<) -c $< -o $@


.SECONDEXPANSION:
$(CMX_FROM_PACK): $(BUILD)/%.cmx: $$(PACK_DEP_$$@) | $(BUILD)/%.ml
	@mkdir -p $(@D)
	@echo "Packing [cmx] $*"
	@$(OCAMLFIND)  $(OCAMLOPT) $(OCAMLFLAGS) $(INCLUDE_FLAG_$@) $(PACK_FLAG_$@) -pack $+ -o $@

$(CMO_FROM_PACK): $(BUILD)/%.cmo: $$(PACK_DEP_$$@) | $(BUILD)/%.ml
	@mkdir -p $(@D)
	@echo "Packing [cmo] $*"
	@$(OCAMLFIND)  $(OCAMLC) $(OCAMLFLAGS) $(INCLUDE_FLAG_$@) $(PACK_FLAG_$@) -pack $+ -o $@

$(ML_OF_PACKS): $(BUILD)/%.ml : $(SRC)/%
	@mkdir -p $(@D)
	@touch $@


####################
## Lex/Yacc rules ##
####################


$(ML_OF_MLL): $(BUILD)/%.ml: $(SRC)/%.mll
	@mkdir -p $(@D)
	@echo "Compiling [ml] $<"
	$(OCAMLLEX) -q $< -o $@

$(ML_OF_MLY): $(BUILD)/%.ml: $(SRC)/%.mly | $(BUILD)/%.dep
	@mkdir -p $(@D)
	@echo "Compiling [ml] $<"
	$(MENHIR)  --explain  $< --base $(BUILD)/$*

$(MLI_OF_MLY): $(BUILD)/%.mli: $(SRC)/%.mly | $(BUILD)/%.dep
	@echo "Compiling [mli] $<"
	$(MENHIR)  --explain  $< --base $(BUILD)/$*

$(CMX_FROM_MLL): %.cmx: %.ml | %.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmx] $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) -c $< -o $@

$(CMO_FROM_MLL): %.cmo: %.ml | %.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmo] $<"
	@$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMA) -c $< -o $@

$(CMX_FROM_MLY): %.cmx: %.ml %.cmi | %.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmx] $<"
	@$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) -I $(BUILD) -c $< -o $@

$(CMO_FROM_MLY): %.cmo: %.ml %.cmi | %.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmo] $<"
	@$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMA) -I $(BUILD) -c $< -o $@

$(CMI_FROM_MLY): %.cmi: %.mli | %.dep
	@mkdir -p $(@D)
	@echo "Compiling [cmi] $<"
	@$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) -I $(BUILD) -c $< -o $@


########################
## Dependencies rules ##
########################

$(DEPS_ML): $(BUILD)/%.dep: $(SRC)/%.ml | $(ML_OF_PACKS) $(ML_OF_MLL) $(ML_OF_MLY)
	@mkdir -p $(@D)
	@echo "Generating dependencies for $<"
	@$(OCAMLFIND) $(OCAMLDEP) -native $(OCAMLINC) $(INCLUDE_FLAG_$<) $(INCLUDE_FLAG_$<:$(BUILD)%=$(SRC)%) $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@


$(DEPS_MLI): $(BUILD)/%.idep: $(SRC)/%.mli | $(ML_OF_PACKS) $(ML_OF_MLL) $(ML_OF_MLY)
	@mkdir -p $(@D)
	@echo "Generating dependencies for $<"
	@$(OCAMLFIND) $(OCAMLDEP) -native $(OCAMLINC) $(INCLUDE_FLAG_$<) $(INCLUDE_FLAG_$<:$(BUILD)%=$(SRC)%) $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@

$(DEPS_MLL): $(BUILD)/%.dep: $(BUILD)/%.ml | $(ML_OF_PACKS) $(ML_OF_MLY)
	@mkdir -p $(@D)
	@echo "Generating dependencies for $<"
	@$(OCAMLFIND) $(OCAMLDEP) -native $(OCAMLINC) -I $(SRC) -I $(BUILD) $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@

$(DEPS_MLY): $(BUILD)/%.dep: $(SRC)/%.mly | $(ML_OF_PACKS)
	@mkdir -p $(@D)
	@echo "Generating dependencies for $<"
	@$(MENHIR) --raw-depend --ocamldep '$(OCAMLFIND) $(OCAMLDEP) $(INCLUDES) -I $(SRC)' $< > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@
