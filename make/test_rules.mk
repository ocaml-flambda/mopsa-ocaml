#####################
# Unit tests rules ##
#####################

tests: $(TESTS_EXE) | tests-deps

tests-deps: $(TESTS_DEP) | $(TESTS_BML)

$(TESTS_BML): $(BTESTS)/%: $(TESTS)/%
	@mkdir -p $(@D)
	@cp $< $@

$(TESTS_DEP): %.dep: %.ml
	@echo "Generating dependencies for $<"
	@$(OCAMLDEP) -I $(BTESTS) -I $(BSRC)  $< > $@

$(TESTS_EXE): %: $(TOP_CMX) $(C_OBJ) $(CC_OBJ) %.cmx
	@echo "Linking $@"
	@$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -package "$(PKGS)" $(LIBCMXA) -linkpkg $+ -o $@

$(TESTS_CMX): %.cmx: $(TOP_CMX) %.ml  | %.dep
	@echo "Compiling $<"
	@$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -package "$(PKGS)" -c -I $(BTESTS) $+ -o $@
