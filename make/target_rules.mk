.PHONY: $(TARGET_NATIVES) $(TARGET_LIBS) $(TARGET_CLIBS)

all: $(TARGET_NATIVES) $(TARGET_LIBS) $(TARGET_CLIBS)

clean:
	-rm -rf $(BUILD)/* $(LIB)/* $(DOC)/*


$(TARGET_NATIVES): %: $(BUILD)/%.native

$(TARGET_LIBS): %: $(BUILD)/%.cmxa

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

ifneq ($(MAKECMDGOALS),clean)
$(foreach target,$(TARGET_NATIVES),$(eval $(call NATIVE_template,$(target))))
$(foreach target,$(TARGET_LIBS),$(eval $(call LIB_template,$(target))))
$(foreach target,$(TARGET_CLIBS),$(eval $(call CLIB_template,$(target))))
endif
