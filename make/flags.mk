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


#########################
## Flags for MLL files ##
#########################

define generate_mll_flags =
 PACK_$(1) = $$(call pack_name,$$(call pack_dir_of_ml,$(1)))
 BASE_$(1) = $$(patsubst $$(SRC)/%.mll,$$(BUILD)/%,$(1))
 CMO_$(1)  = $$(BASE_$(1)).cmo
 CMX_$(1)  = $$(BASE_$(1)).cmx
 ML_$$(CMO_$(1)) = $$(BASE_$(1)).ml
 ML_$$(CMX_$(1)) = $$(BASE_$(1)).ml

 INCLUDES_$(1) = $$(call include_lineage,$$(shell dirname $(1)))
 INCLUDES_$$(CMO_$(1)) = $$(INCLUDES_$(1))
 INCLUDES_$$(CMX_$(1)) = $$(INCLUDES_$(1))

 OCAMLFLAGS_$(1) = \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(INCLUDES_$(1)) $$(if $$(filter $$(PACK_$(1)),.), -c $$(BASE_$(1)).ml ,-for-pack $$(PACK_$(1)) -c $$(BASE_$(1)).ml)
 OCAMLFLAGS_$$(CMO_$(1)) = $$(OCAMLFLAGS_$(1))
 OCAMLFLAGS_$$(CMX_$(1)) = $$(OCAMLFLAGS_$(1))

 DEPFLAGS_$$(BASE_$(1)).ml.dep = \
	$$(INCLUDES_$(1)) \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(BASE_$(1)).ml
endef

$(foreach mll,$(MLL),$(eval $(call generate_mll_flags,$(mll))))


#########################
## Flags for MLY files ##
#########################

define generate_mly_flags =
 PACK_$(1) = $$(call pack_name,$$(call pack_dir_of_ml,$(1)))
 BASE_$(1) = $$(patsubst $$(SRC)/%.mly,$$(BUILD)/%,$(1))

 CMI_$(1)  = $$(BASE_$(1)).cmi
 CMO_$(1)  = $$(BASE_$(1)).cmo
 CMX_$(1)  = $$(BASE_$(1)).cmx

 MLI_$$(CMI_$(1)) = $$(BASE_$(1)).mli
 ML_$$(CMX_$(1)) = $$(BASE_$(1)).ml
 ML_$$(CMO_$(1)) = $$(BASE_$(1)).ml

 INCLUDES_$(1) = $$(call include_lineage,$$(shell dirname $(1)))
 INCLUDES_$$(CMI_$(1)) = $$(INCLUDES_$(1))
 INCLUDES_$$(CMO_$(1)) = $$(INCLUDES_$(1))
 INCLUDES_$$(CMX_$(1)) = $$(INCLUDES_$(1))

 OCAMLFLAGS_$(1) = \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(INCLUDES_$(1)) $$(if $$(filter $$(PACK_$(1)),.), -c $$(BASE_$(1)).ml ,-for-pack $$(PACK_$(1)) -c $$(BASE_$(1)).ml)
 OCAMLFLAGS_$$(CMO_$(1)) = $$(OCAMLFLAGS_$(1))
 OCAMLFLAGS_$$(CMX_$(1)) = $$(OCAMLFLAGS_$(1))

 OCAMLFLAGS_$$(CMI_$(1)) = \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(INCLUDES_$(1)) $$(if $$(filter $$(PACK_$(1)),.), -c $$(BASE_$(1)).mli ,-for-pack $$(PACK_$(1)) -c $$(BASE_$(1)).mli)

 DEPFLAGS_$$(BASE_$(1)).ml.dep = \
	$$(INCLUDES_$(1)) \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(BASE_$(1)).ml
 DEPFLAGS_$$(BASE_$(1)).mli.dep = \
	$$(INCLUDES_$(1)) \
	$$(patsubst $$(SRC)%,$$(BUILD)%,$$(INCLUDES_$(1))) \
	$$(BASE_$(1)).mli
endef

$(foreach mly,$(MLY),$(eval $(call generate_mly_flags,$(mly))))


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
