.PHONY: $(TARGET_NATIVES) $(TARGET_LIBS) $(TARGET_CLIBS)

all: $(TARGET_NATIVES) $(TARGET_LIBS) $(TARGET_CLIBS)

$(TARGET_NATIVES): %: $(BUILD)/%.native
$(TARGET_LIBS): %: $(BUILD)/%.cmxa
$(TARGET_CLIBS): %: $(BUILD)/lib%.a


clean:
	-rm -rf $(BUILD)/* $(BIN)/* $(DOC)/*


.SECONDEXPANSION:

$(BUILD)/%.native: $$(call top_cmx, %) | deps
	@echo "Linking native binary"
	@$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -cclib "$(LDFLAGS)" -package "$(PKGS)" -linkpkg  $(LIBCMXA) $+ -o $@
	@mkdir -p $(BIN)
	@cp $(BUILD)/*.* $(BIN)

$(BUILD)/%.cmxa: $$(call top_cmx, %) | deps
	@echo "Linking native library $@ $+"
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -cclib "$(LDFLAGS)" -a -o $@ -package "$(PKGS)" $+
	@mkdir -p $(BIN)
	@cp $(BUILD)/*.* $(BSRC)/*.cmx $(BSRC)/*.cmt $(BSRC)/*.cmi $(BIN)

$(BUILD)/lib%.a: $$(call top_cmx, %) $(C_OBJ) $(CC_OBJ) | deps
	@echo "Linking native/C library"
	@$(OCAMLMKLIB) -o $@ -ocamlc "$(OCAMLC)" -ocamlopt "$(OCAMLOPT)" $(LDFLAGS) $(LIBS) $(TOP_CMX)
	@$(OCAMLMKLIB) -o $@ -ocamlc "$(OCAMLC)" -ocamlopt "$(OCAMLOPT)" $(LDFLAGS) $(LIBS) $(C_OBJ) $(CC_OBJ)
	@mkdir -p $(BIN)
	@cp $(BUILD)/*.* $(BSRC)/*.cmx $(BSRC)/*.cmi $(BSRC)/*.cmt $(BIN)
