define generate_merlin_paths =
 ROOT_$(1) = $$(call merlin_root_path,$(1))
 MERLIN_PATHS_$(1) = \
	S $$(ROOT_$(1))/src/** \n\
	$$(foreach p,$$(call merlin_lineage,$(1)),B $$(p)\n)\
	$$(foreach p,$$(filter-out -I $$(BUILD),$$(INCLUDES)),B $$(ROOT_$(1))/$$(p)/**\n)
endef


$(foreach m,$(MERLIN),$(eval $(call generate_merlin_paths,$(m))))

merlin: $(MERLIN)

clean-merlin:
	-rm $(MERLIN)

$(MERLIN):
	@echo "[GEN]	$@"
	$(shell echo -e "$(MERLIN_PATHS_$@)" >> $@)
	$(file >>$@,PKG $(PKGS))
