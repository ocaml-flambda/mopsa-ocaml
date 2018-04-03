native: $(BTARGET).native merlin

$(BTARGET).native: $(TOP_CMX) | deps
	@echo "Linking native binary"
	@$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -cclib "$(LDFLAGS)" -package "$(PKGS)" -linkpkg  $(LIBCMXA) $+ -o $(BTARGET).native
	@mkdir -p $(BIN)
	@cp $(BUILD)/*.* $(BIN)

lib-native: $(BTARGET).cmxa merlin

$(BTARGET).cmxa: $(TOP_CMX) | deps
	@echo "Linking native library"
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLFLAGS) -a -o $(BTARGET).cmxa -package "$(PKGS)" $+
	@mkdir -p $(BIN)
	@cp $(BUILD)/*.* $(BSRC)/*.cmx $(BSRC)/*.cmt $(BSRC)/*.cmi $(BIN)

lib-native-c: $(BUILD)/lib$(TARGET).a merlin

$(BUILD)/lib$(TARGET).a: $(TOP_CMX) $(C_OBJ) $(CC_OBJ) | deps
	@echo "Linking native/C library"
	@$(OCAMLMKLIB) -o $(BTARGET) -ocamlc "$(OCAMLC)" -ocamlopt "$(OCAMLOPT)" $(LDFLAGS) $(LIBS) $(TOP_CMX)
	@$(OCAMLMKLIB) -o $(BTARGET) -ocamlc "$(OCAMLC)" -ocamlopt "$(OCAMLOPT)" $(LDFLAGS) $(LIBS) $(C_OBJ) $(CC_OBJ)
	@mkdir -p $(BIN)
	@cp $(BUILD)/*.* $(BSRC)/*.cmx $(BSRC)/*.cmi $(BSRC)/*.cmt $(BIN)

clean:
	-rm -rf $(BUILD)/* $(BIN)/* $(DOC)/*
