#################
## C/C++ rules ##
#################

$(C_OBJ): $(BUILD)/%.o: $(SRC)/%.c
	@echo "[CC ]  $<"
	@$(CC) $(CFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@

$(CC_OBJ): $(BUILD)/%.o: $(SRC)/%.cc
	@echo "[CPP] $<"
	@$(CXX) $(CXXFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@
