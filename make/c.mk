#################
## C/C++ rules ##
#################

$(C_OBJ): $(BUILD)/%.o: $(SRC)/%.c
	@echo "Compiling [o] $<"
	@$(CC) $(CFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@

$(CC_OBJ): $(BUILD)/%.o: $(SRC)/%.cc
	@echo "Compiling [o] $<"
	@$(CXX) $(CXXFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@
