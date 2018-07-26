##################################
## Generate flags for each pack ##
##################################

define generate_pack_flags =
 PACK_NAME_$(1) = $$(call pack_name,$(1))
 PACK_CMX_$(1) = $$(BUILD)/$(1).cmx
 INCLUDE_FLAG_$$(PACK_CMX_$(1)) = $$(patsubst $$(SRC)%,$$(BUILD)%, $$(call include_lineage,$$(SRC)/$(1)))
 PACK_CONTENT_$(1) = \
	$$(if $$($(1)),\
		$$($(1)),\
		$$(foreach f,$$(wildcard $$(SRC)/$(1)/*.ml) $$(shell find $$(SRC)/$(1)/* -maxdepth 0 -type d),$$(shell basename $$(f) .ml))\
	)
 PACK_DEP_$$(PACK_CMX_$(1)) = $$(patsubst %,$$(BUILD)/$(1)/%.cmx,$$(PACK_CONTENT_$(1)))
 PARENT_PACK_$(1) = $$(patsubst /%,%,$$(patsubst $$(SRC)%,%,$$(shell realpath --relative-to=. $$(SRC)/$(1)/..)))
 PACK_FLAG_$$(PACK_CMX_$(1)) = \
	$$(if $$(PARENT_PACK_$(1)),\
		-for-pack $$(call pack_name,$$(PARENT_PACK_$(1))),\
		\
	)
endef

$(foreach pack,$(PACKS),$(eval $(call generate_pack_flags,$(pack))))

#####################################
## Generate flags for each ml file ##
#####################################

define generate_ml_flags =
 INCLUDE_FLAG_$(1) = $$(patsubst $$(SRC)%,$$(BUILD)%,$$(call include_lineage,$$(shell dirname $(1))))
 PACK_NAME_$(1) = $$(call pack_name,$$(call pack_dir_of_ml,$(1)))
 PACK_FLAG_$(1) = $$(if $$(filter $$(PACK_NAME_$(1)),.), ,-for-pack $$(PACK_NAME_$(1)))
endef

$(foreach ml,$(ML),$(eval $(call generate_ml_flags,$(ml))))
$(foreach mli,$(MLI),$(eval $(call generate_ml_flags,$(mli))))
