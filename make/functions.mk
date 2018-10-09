lib_src_dir = $(shell $(SED) -e "s@\([^:]*\):.*@$(MOPSAROOT)/\1/src@g" <<< $(1))

lib_dir = $(shell $(SED) -e "s@\([^:]*\):.*@$(MOPSAROOT)/\1/lib@g" <<< $(1))

lib_file = $(shell $(SED) -e "s@\([^:]*\):\(.*\)@$(MOPSAROOT)/\1/lib/\2@g" <<< $(1))

lineage = \
	$(if $(filter $(1), $(SRC)), \
		$(1), \
		$(1) $(call lineage,$(shell realpath --relative-to=. $(1)/..))\
	)

include_lineage = $(foreach p,$(call lineage,$(1)),-I $(p))

is_directory = $(shell test -d $(basename $(1)) && echo 1 || echo 0)

pack_dir_of_ml = $(shell dirname $(patsubst $(SRC)/%,%,$(1)))

pack_name = $(subst /,.,$(shell $(SED) -e "s/\b\(.\)/\u\1/g" <<< $(1)))

merlin_root_path = $(shell $(SED) -e "s/[^\/]\+\//\.\.\//g" -e "s/\/[^\/]\+$$//g" <<< $(1))

merlin_lineage = $(patsubst $(SRC)%,$(call merlin_root_path,$(1))/_build%,$(call lineage,$(shell dirname $(1))))
