####################
## Lex/Yacc rules ##
####################
$(BMLL) $(BMLY): $(BSRC)/%: $(SRC)/%
	@mkdir -p $(@D)
	@cp $< $@

.PRECIOUS: $(BMLL:%.mll=%.cmi) $(BMLL:%.mll=%.mli)
$(BSMLL): %.ml: %.mll
	@echo "Generating $@"
	@$(OCAMLLEX) -q $<

.PRECIOUS: $(BMLY:%.mly=%.cmi) $(BMLY:%.mly=%.mli)
$(BSMLY): %.ml: %.mly | %.ydep
	@echo "Generating $@"
	@$(MENHIR) --ocamlc '$(OCAMLFIND) $(OCAMLC) $(INCLUDES) -package "$(PKGS)" -bin-annot -I $(BSRC)' --explain --infer $<

%.ydep: %.mly | $(ALL_ML)
	@echo "Generating dependencies for $<"
	@$(MENHIR) --raw-depend --ocamldep '$(OCAMLFIND) $(OCAMLDEP) $(INCLUDES)' $< > $@
