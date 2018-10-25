#################
## C/C++ rules ##
#################

$(C_OBJ): $(BUILD)/%.o: $(SRC)/%.c
	@echo "[CC]  $<"
	$(QUIET)$(CC) $(CFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@

$(CC_OBJ): $(BUILD)/%.o: $(SRC)/%.cc
	@echo "[CXX] $<"
	$(QUIET)$(CXX) $(CXXFLAGS) -fPIC -I $(shell ocamlc -where) -c $< -o $@
