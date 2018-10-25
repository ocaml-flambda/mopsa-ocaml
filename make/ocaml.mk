########################
## Flags for ML files ##
########################

define generate_ml_flags =
 PACK_$(1) = $$(call pack_name,$$(call pack_dir_of_ml,$(1)))
 BASE_$(1) = $$(patsubst $$(SRC)/%.ml,$$(BUILD)/%,$(1))
 CMO_$(1)  = $$(BASE_$(1)).cmo
 CMX_$(1)  = $$(BASE_$(1)).cmx
 ML_$$(CMO_$(1)) = $(1)
 ML_$$(CMX_$(1)) = $(1)

 INCLUDES_$(1) = $$(call include_lineage,$$(shell dirname $(1)))
 INCLUDES_$$(CMO_$(1)) = $$(INCLUDES_$(1))
 INCLUDES_$$(CMX_$(1)) = $$(INCLUDES_$(1))

 OCAMLFLAGS_$(1) = \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(INCLUDES_$(1)) $$(if $$(filter $$(PACK_$(1)),.), -c $(1) ,-for-pack $$(PACK_$(1)) -c $(1))
 OCAMLFLAGS_$$(CMO_$(1)) = $$(OCAMLFLAGS_$(1))
 OCAMLFLAGS_$$(CMX_$(1)) = $$(OCAMLFLAGS_$(1))

 DEPFLAGS_$$(BASE_$(1)).ml.dep = \
	$$(INCLUDES_$(1)) \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$(1)
endef

$(foreach ml,$(ML),$(eval $(call generate_ml_flags,$(ml))))


#########################
## Flags for MLI files ##
#########################

define generate_mli_flags =
 PACK_$(1) = $$(call pack_name,$$(call pack_dir_of_ml,$(1)))
 BASE_$(1) = $$(patsubst $$(SRC)/%.mli,$$(BUILD)/%,$(1))
 CMI_$(1)  = $$(BASE_$(1)).cmi
 MLI_$$(CMI_$(1)) = $(1)

 INCLUDES_$(1) = $$(call include_lineage,$$(shell dirname $(1)))
 INCLUDES_$$(CMI_$(1)) = $$(INCLUDES_$(1))

 OCAMLFLAGS_$(1) = \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	-c $(1)
 OCAMLFLAGS_$$(CMI_$(1)) = $$(OCAMLFLAGS_$(1))

 DEPFLAGS_$$(BASE_$(1)).mli.dep = \
	$$(INCLUDES_$(1)) \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$(1)
endef

$(foreach mli,$(MLI),$(eval $(call generate_mli_flags,$(mli))))


#####################
## Flags for packs ##
#####################

define generate_pack_flags =
 PACK_$(1) = $$(call pack_name,$(1))
 PACK_VAR_$(1) =  $$(subst /,.,$(1))
 CMX_$(1) = $$(BUILD)/$(1).cmx
 CMO_$(1) = $$(BUILD)/$(1).cmo

 ML_$$(CMO_$(1)) = $(1)
 ML_$$(CMX_$(1)) = $(1)

 INCLUDES_$(1) = $$(patsubst $$(SRC)%,$$(BUILD)%, $$(call include_lineage,$$(SRC)/$(1)))
 INCLUDES_$$(CMO_$(1)) = $$(INCLUDES_$(1))
 INCLUDES_$$(CMX_$(1)) = $$(INCLUDES_$(1))

 PACK_MODULES_$(1) = \
	$$(if $$($$(PACK_VAR_$(1))),\
		$$($$(PACK_VAR_$(1))),\
		$$(foreach f,$$(wildcard $$(SRC)/$(1)/*.ml) $$(shell find $$(SRC)/$(1)/* -maxdepth 0 -type d),$$(shell basename $$(f) .ml))\
	)
 PACK_DEPS_$$(CMX_$(1)) = $$(patsubst %,$$(BUILD)/$(1)/%.cmx,$$(PACK_MODULES_$(1)))
 PACK_DEPS_$$(CMO_$(1)) = $$(patsubst %,$$(BUILD)/$(1)/%.cmo,$$(PACK_MODULES_$(1)))

 PARENT_PACK_$(1) = $$(patsubst /%,%,$$(patsubst $$(SRC)%,%,$$(shell realpath --relative-to=. $$(SRC)/$(1)/..)))
 OCAMLFLAGS_$$(CMX_$(1)) = \
	$$(PACK_DEPS_$$(CMX_$(1))) \
	-pack $$(PACK_NAME_$(1)) \
	$$(if $$(PARENT_PACK_$(1)),\
		-for-pack $$(call pack_name,$$(PARENT_PACK_$(1))),\
		\
	)
 OCAMLFLAGS_$$(CMO_$(1)) = \
	$$(PACK_DEPS_$$(CMO_$(1))) \
	-pack $$(PACK_NAME_$(1)) \
	$$(if $$(PARENT_PACK_$(1)),\
		-for-pack $$(call pack_name,$$(PARENT_PACK_$(1))),\
		\
	)
endef

$(foreach pack,$(PACKS),$(eval $(call generate_pack_flags,$(pack))))


##################
## Dependencies ##
##################

$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmi): $(BUILD)/%.cmi: $(SRC)/%.mli | $(BUILD)/%.mli.dep

$(ML:$(SRC)/%.ml=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(SRC)/%.ml | $(BUILD)/%.ml.dep
$(ML:$(SRC)/%.ml=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(SRC)/%.ml | $(BUILD)/%.ml.dep

$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.cmi
$(MLI:$(SRC)/%.mli=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.cmi

$(MLL:$(SRC)/%.mll=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml
$(MLL:$(SRC)/%.mll=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.ml
$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmx): $(BUILD)/%.cmx: $(BUILD)/%.ml | $(BUILD)/%.ydep
$(MLY:$(SRC)/%.mly=$(BUILD)/%.cmo): $(BUILD)/%.cmo: $(BUILD)/%.ml | $(BUILD)/%.ydep

$(ML:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%
$(MLI:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%
$(MLL:$(SRC)/%.mll=$(BUILD)/%.ml.dep): $(BUILD)/%.ml.dep: $(BUILD)/%.ml
$(MLY:$(SRC)/%=$(BUILD)/%.dep): $(BUILD)/%.dep: $(SRC)/%

.SECONDEXPANSION:
$(PACKS:%=$(BUILD)/%.cmx): $(BUILD)/%.cmx : $$(PACK_DEPS_$$@)
$(PACKS:%=$(BUILD)/%.cmo): $(BUILD)/%.cmo : $$(PACK_DEPS_$$@)

$(PACKS:%=$(BUILD)/%.ml): $(BUILD)/%.ml : $(SRC)/%
	@mkdir -p $(@D)
	@echo "[GEN] $@"
	$(QUIET)touch $@


######################
## OCamlLex recipes ##
######################


$(MLL:$(SRC)/%.mll=$(BUILD)/%.ml):
	@mkdir -p $(@D)
	@echo "[MLL] $<"
	$(QUIET)$(OCAMLLEX) -q $< -o $@


####################
## Menhir recipes ##
####################

$(MLY:$(SRC)/%.mly=$(BUILD)/%.ml) $(MLY:$(SRC)/%.mly=$(BUILD)/%.mli):
	@mkdir -p $(@D)
	@echo "[MLY] $<"
	$(QUIET)$(MENHIR)  --explain  $< --base `dirname $@`/`basename $@ .ml`



###################
## Ocaml recipes ##
###################

%.cmx:
	@mkdir -p $(@D)
	@echo "[CMX] $(ML_$@)"
	$(QUIET)$(OCAMLFIND)  $(OCAMLOPT) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMXA) $(OCAMLFLAGS_$@) -o $@

%.cmo:
	@mkdir -p $(@D)
	@echo "[CMO] $(ML_$@)"
	$(QUIET)$(OCAMLFIND) $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(LIBCMA) $(OCAMLFLAGS_$@) -o $@

%.cmi:
	@mkdir -p $(@D)
	@echo "[CMI] $(MLI_$@)"
	$(QUIET)$(OCAMLFIND)  $(OCAMLC) -package "$(PKGS)" $(OCAMLFLAGS) $(OCAMLFLAGS_$@) -o $@

%.ml.dep %.mli.dep: | $(PACKS:%=$(BUILD)/%.ml)
	@mkdir -p $(@D)
	@echo "[DEP] $@"
	$(QUIET)$(OCAMLFIND) $(OCAMLDEP) -absname $(DEPFLAGS_$@) > $@
	@$(SED) -i 's/\bsrc\b/_build/g' $@

%.mly.dep: | $(PACKS:%=$(BUILD)/%.ml)
	@mkdir -p $(@D)
