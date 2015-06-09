# testing makefiles

CC = clang
CLANGDUMP = clang -x c -Xclang -ast-dump -fsyntax-only -fno-color-diagnostics
CFLAGS = -std=c11
LFLAGS =
DEPS = test_header.h
GEN = test_header.h test_header.c
TMP_GEN = test_header.h.tt test_header.c.tt
OBJ = main.o test_header.o extra_header.o

# generated files -- keep these as single files!
SYNTAX_DUMP = syn_dump.ast
VARS = global_vars.yaml
STRUCTS = global_structs.yaml
ALL = everything.yaml
# temporary files
UNIQUE_FILES = .unique_files
GEN_C_FILES = .gen_c_files

%.h.tt: %.tmplh
	cp $^ $@

%.c.tt: %.tmplc
	cp $^ $@

%.h: %.tmplh
	./in.pl $(VARS) $(STRUCTS) $^ $@

%.c: %.tmplc
	./in.pl $(VARS) $(STRUCTS) $^ $@
	./insert_headers_gen.pl $@
	./cleanup_ttmp.sh $@

%.o: %.c $(DEPS)
	$(CC) -c -o $@ -DREPLACE_FOR_GEN $< $(CFLAGS)

main: $(VARS) $(OBJ) $(DEPS) $(GEN)
	$(CC) -o $@ $(OBJ) $(LFLAGS)

$(SYNTAX_DUMP): $(TMP_GEN)
	find . -name "*.tt" -exec ./rename_tmp.sh '{}' $(GEN_C_FILES) ';'
	echo $(wildcard *.c) $(GEN) > $(UNIQUE_FILES)
	sort $(UNIQUE_FILES) | uniq | xargs $(CLANGDUMP) > $@
	./remove_files.sh $(GEN_C_FILES)
	rm $(UNIQUE_FILES)
	rm $(GEN_C_FILES)

$(STRUCTS): $(SYNTAX_DUMP)
	./get_struct_decls.sh "$(SYNTAX_DUMP)" > $(STRUCTS)

$(VARS): $(STRUCTS)
	./get_var_decls.sh "$(SYNTAX_DUMP)" > $(VARS)
	./get_struct_as_json.pl "$(SYNTAX_DUMP)" > $(ALL)

.PHONY: clean
clean:
	rm -f *.o
	rm -f *.ttmp
	rm -f *.tt
	rm -f $(GEN)
	rm -f $(GEN_C_FILES)
	rm -f $(TMP_GEN)
	rm -f $(STRUCTS)
	rm -f $(SYNTAX_DUMP)
	rm -f $(VARS)
	rm -f $(ALL)
	rm -f $(UNIQUE_FILES)
	rm -f main
