#################
## C/C++ rules ##
#################

$(C_OBJ): $(BUILD)/%.o: $(SRC)/%.c
	@echo -e "$(CCMSG)	$<"
	$(QUIET)$(CC) $(CFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@

$(CC_OBJ): $(BUILD)/%.o: $(SRC)/%.cc
	@echo -e "$(CXXMSG)	$<"
	$(QUIET)$(CXX) $(CXXFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@
