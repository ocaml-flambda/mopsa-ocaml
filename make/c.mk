#################
## C/C++ rules ##
#################

$(C_OBJ): $(BUILD)/%.o: $(SRC)/%.c
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@

$(CC_OBJ): $(BUILD)/%.o: $(SRC)/%.cc
	@echo "Compiling $<"
	@$(CXX) $(CXXFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@
