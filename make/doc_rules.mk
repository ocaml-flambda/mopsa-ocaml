doc: $(TOP_CMX:%.cmx=%.ml)
	@mkdir -p doc
	@echo "Generating documentation"
	@$(OCAMLFIND) $(OCAMLDOC) -package "$(PKGS)" $(INCLUDES) -d $(DOC) -html -colorize-code -charset utf8 $+

