.PHONY: $(TARGET_NATIVES) $(TARGET_LIBS) $(TARGET_BYTES) $(TARGET_BYTELIBS) $(TARGET_CLIBS)

all: $(MERLIN) $(TARGET_NATIVES) $(TARGET_LIBS) $(TARGET_CLIBS)

clean:
	-rm -rf $(BUILD)/* $(LIB)/* $(MERLIN)


$(TARGET_NATIVES): %: $(BUILD)/%.native

$(TARGET_LIBS): %: $(BUILD)/%.cmxa

$(TARGET_NATIVES): %: $(BUILD)/%.byte

$(TARGET_LIBS): %: $(BUILD)/%.cma

$(TARGET_CLIBS): %: $(BUILD)/lib%.a


define NATIVE_template =
 TOP_CMX_$(1) := $$(if $$($(1)), $$($(1):%=$$(BUILD)/%.cmx), $$(TOPML:%.ml=$$(BUILD)/%.cmx) $$(TOPPACKS:%=$$(BUILD)/%.cmx))
 TARGET_$(1) = $$(BUILD)/$(1).native

 $$(TARGET_$(1)): $$(TOP_CMX_$(1))
	@echo "Linking native binary $(1)"
	@$$(OCAMLFIND) $$(OCAMLOPT) $$(OCAMLFLAGS) -cclib "$$(LDFLAGS)" -package "$$(PKGS)" -linkpkg  $$(LIBCMXA) $$+ -o $$@
endef


define LIB_template =
 TOP_CMX_$(1) := $$(if $$($(1)), $$($(1):%=$$(BUILD)/%.cmx), $$(TOPML:%.ml=$$(BUILD)/%.cmx) $$(TOPPACKS:%=$$(BUILD)/%.cmx))
 TARGET_$(1) = $$(BUILD)/$(1).cmxa

 $$(TARGET_$(1)): $$(TOP_CMX_$(1))
	@echo "Linking native library $(1)"
	@$$(OCAMLFIND) $$(OCAMLOPT) $$(OCAMLFLAGS) -cclib "$$(LDFLAGS)" -a -o $$@ -package "$$(PKGS)" $$+
	@mkdir -p $$(LIB)
	@cp $$(BUILD)/*.*  $$(LIB)
endef


define CLIB_template =
 TOP_CMX_$(1) := $$(if $$($(1)), $$($(1):%=$$(BUILD)/%.cmx), $$(TOPML:%.ml=$$(BUILD)/%.cmx) $$(TOPPACKS:%=$$(BUILD)/%.cmx))
 TARGET_$(1) = $$(BUILD)/lib$(1).a
 TARGET_BASE_$(1) = $$(BUILD)/$(1)

 $$(TARGET_$(1)): $$(TOP_CMX_$(1)) $$(C_OBJ) $$(CC_OBJ)
	@echo "Linking native/C library $(1)"
	@$$(OCAMLMKLIB) -o $$(TARGET_BASE_$(1)) -ocamlc "$$(OCAMLC)" -ocamlopt "$$(OCAMLOPT)" $$(LDFLAGS) $$(CCLIBS) $$(TOP_CMX_$(1))
	@$$(OCAMLMKLIB) -o $$(TARGET_BASE_$(1)) -ocamlc "$$(OCAMLC)" -ocamlopt "$$(OCAMLOPT)" $$(LDFLAGS) $$(CCLIBS) $$(C_OBJ) $$(CC_OBJ)
	@mkdir -p $$(LIB)
	@cp $$(BUILD)/*.*  $$(LIB)

endef


define BYTE_template =
 TOP_CMO_$(1) := $$(if $$($(1)), $$($(1):%=$$(BUILD)/%.cmo), $$(TOPML:%.ml=$$(BUILD)/%.cmo) $$(TOPPACKS:%=$$(BUILD)/%.cmo))
 TARGET_$(1) = $$(BUILD)/$(1).byte

 $$(TARGET_$(1)): $$(TOP_CMO_$(1))
	@echo "Linking bytecode binary $(1)"
	@$$(OCAMLFIND) $$(OCAMLC) $$(OCAMLFLAGS) -cclib "$$(LDFLAGS)" -package "$$(PKGS)" -linkpkg $$(LIBCMA) $$(DLLPATHS) $$+ -o $$@
endef


define BYTELIB_template =
 TOP_CMO_$(1) := $$(if $$($(1)), $$($(1):%=$$(BUILD)/%.cmo), $$(TOPML:%.ml=$$(BUILD)/%.cmo) $$(TOPPACKS:%=$$(BUILD)/%.cmo))
 TARGET_$(1) = $$(BUILD)/$(1).cma

 $$(TARGET_$(1)): $$(TOP_CMO_$(1))
	@echo "Linking bytecode library $(1)"
	@$$(OCAMLFIND) $$(OCAMLC) $$(OCAMLFLAGS) -cclib "$$(LDFLAGS)" -a -o $$@ -package "$$(PKGS)" $$+
	@mkdir -p $$(LIB)
	@cp $$(BUILD)/*.*  $$(LIB)
endef


define BYTECLIB_template =
 TOP_CMO_$(1) := $$(if $$($(1)), $$($(1):%=$$(BUILD)/%.cmo), $$(TOPML:%.ml=$$(BUILD)/%.cmo) $$(TOPPACKS:%=$$(BUILD)/%.cmo))
 CTARGET_$(1) = $$(BUILD)/lib$(1).a
 TARGET_$(1) = $$(BUILD)/$(1).cma
 TARGET_BASE_$(1) = $$(BUILD)/$(1)

# compiling the bytecode version requires compiling the native version, for the .a / .so
# TODO: allow compiling bytecode only (or native only)
 $$(CTARGET_$(1)): $$(TARGET_$(1))

 $$(TARGET_$(1)): $$(TOP_CMO_$(1)) $$(C_OBJ) $$(CC_OBJ)
	@echo "Linking bytecode/C library $(1)"
	@$$(OCAMLMKLIB) -o $$(TARGET_BASE_$(1)) -ocamlc "$$(OCAMLC)" -ocamlopt "$$(OCAMLOPT)" $$(LDFLAGS) $$(CCLIBS) $$(TOP_CMO_$(1))
	@mkdir -p $$(LIB)
	@cp $$(BUILD)/*.*  $$(LIB)

endef

ifneq ($(MAKECMDGOALS),clean)
$(foreach target,$(TARGET_NATIVES),$(eval $(call NATIVE_template,$(target))))
$(foreach target,$(TARGET_LIBS),$(eval $(call LIB_template,$(target))))
$(foreach target,$(TARGET_CLIBS),$(eval $(call CLIB_template,$(target))))
$(foreach target,$(TARGET_NATIVES),$(eval $(call BYTE_template,$(target))))
$(foreach target,$(TARGET_LIBS),$(eval $(call BYTELIB_template,$(target))))
$(foreach target,$(TARGET_CLIBS),$(eval $(call BYTECLIB_template,$(target))))
endif
