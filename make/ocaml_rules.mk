#################
## Ocaml rules ##
#################

## Copy ml sources into build directory to get the right paths when running ocamldep
$(BML) $(BMLI): $(BSRC)/%: $(SRC)/%
	@mkdir -p $(@D)
	@cp $< $@

%.cmx: %.ml | %.dep
	@echo "Compiling $<"
	@$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -package "$(PKGS)" $(LIBCMXA) -c $< -o $@

%.cmo: %.ml %.cmi | %.dep
	@echo "Compiling $<"
	@$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -package "$(PKGS)" $(LIBCMA) -c $< -o $@

%.cmi: %.mli
	@echo "Compiling $<"
	@$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -package "$(PKGS)" -o $@ -c $<

# [AM] commented-out: this caused a given .mli file to be overwitten with
# a generated one in case of compilation error!
#
#%.mli: %.ml
#	@echo "Generating $@"
#	@$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) -package "$(PKGS)" -i $(@:%.mli=%.ml) > $@
