########################
## Dependencies rules ##
########################

deps: $(BML:%.ml=%.dep) | $(ALL_ML)

%.dep: %.ml | $(ALL_ML)
	@echo "Generating dependencies for $<"
	@$(OCAMLDEP) $(INCLUDES)  $< > $@
