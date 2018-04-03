# This template function generates for each pack the necessary recipes to compile its contents
define PACK_template =
 #Some definitions
 SPATH_$(1) = $$(SRC)/$$(subst .,/,$(1))
 BPATH_$(1) = $$(SPATH_$(1):$$(SRC)/%=$$(BSRC)/%)

 #Pack name with capitalized parts
 NAME_$(1) = $$(shell sed -e "s/\b\(.\)/\u\1/g" <<< $(1))

 #Include directories
 INCLUDES_$(1) = $$(subst -I $$(SRC),-I $$(BSRC),$$(call includes,$$(SPATH_$(1))))

 #Sub-sources
 SML_$(1) = $$(wildcard $$(SPATH_$(1))/*.ml)
 BML_$(1) = $$(SML_$(1):$$(SRC)/%=$$(BSRC)/%)

 #Dependencies of sub-sources to be generated
 DEPS_$(1) = $$(BML_$(1):%.ml=%.dep)

 #Object files
 CMO_$(1) = $$(BML_$(1):%.ml=%.cmo)
 CMX_$(1) = $$(BML_$(1):%.ml=%.cmx)

 #Contents
 LS_$(1) = \
	$$(if $$($(1)),\
		$$($(1)),\
		$$(SML_$(1):$$(SPATH_$(1))/%.ml=%) $$(call subdirs,$$(SPATH_$(1)))\
	)

 #Parent pack
 PARENT_$(1) = $$(subst /,.,$$(filter-out $$(BSRC), $$(patsubst $$(BSRC)/%, %, $$(shell realpath --relative-to=. $$(BPATH_$(1))/..))))
 PARENT_PACK_$(1) = $$(if $$(PARENT_$(1)), -for-pack $$(shell sed -e "s/\b\(.\)/\u\1/g" <<< $$(PARENT_$(1))), )

 #Merlin prefix
 MERLIN_PREFIX_$(1) = $$(shell sed -e "s/[^\/]*/../g" <<< $$(SPATH_$(1)))

 #Generate pack ml using ocp-pack.
 #Need to put an empty comment line at the beginning to get proper documentation.
 $$(BPATH_$(1)).ml: $$(LS_$(1):%=$$(BPATH_$(1))/%.ml)
	@mkdir -p $$(@D)
	@$$(OCPPACK) -o $$@ $$+
	@$$(SED) -i '1s/^/(** *)\n/' $$@

 #Generate dependency files of the sub-sources in the pack
 $$(DEPS_$(1)): %.dep: %.ml | $$(ALL_ML)
	@echo "Generating dependencies of $$<"
	@$$(OCAMLDEP) $$(INCLUDES_$(1)) $$< > $$@

 #Compile the sub-sources with the right packing flags
 $$(CMX_$(1)): %.cmx: %.ml | %.dep
	@echo "Compiling $$<"
	$$(OCAMLFIND)  $$(OCAMLOPT) -package "$$(PKGS)" $$(OCAMLFLAGS) -c -for-pack $$(NAME_$(1)) $$(INCLUDES_$(1)) $$(LIBCMXA) $$< -o $$@

 #Compile the pack
 $$(BPATH_$(1)).cmo: %.cmo: $$(LS_$(1):%=$$(BPATH_$(1))/%.cmo) | %.ml
	@echo "Packing $$@"
	@$$(OCAMLFIND) $$(OCAMLC) $$(OCAMLFLAGS) -package "$$(PKGS)" -pack $$(PARENT_PACK_$(1)) $$(INCLUDES_$(1)) $$+ -o $$@

 $$(BPATH_$(1)).cmx: %.cmx: $$(LS_$(1):%=$$(BPATH_$(1))/%.cmx) | %.ml
	@echo "Packing $$@"
	@$$(OCAMLFIND) $$(OCAMLOPT) $$(OCAMLFLAGS) -package "$$(PKGS)" -pack $$(PARENT_PACK_$(1)) $$(INCLUDES_$(1))  $$+ -o $$@

 merlin: $$(SPATH_$(1))/.merlin

 $$(SPATH_$(1))/.merlin:
	@echo "Generating .merlin for $(1)"
	$$(foreach \
		path,\
		$$(filter-out -I,$$(INCLUDES) $$(INCLUDES_$(1))),\
		$$(file >>$$@,B $$(MERLIN_PREFIX_$(1))/$$(path))\
	)
	$$(file >>$$@,PKG $$(PKGS))
endef
