# example makefile for using this script

# choose compiler
CC = clang

# .h files
DEPS =

# put in all our .c files as .o files
OBJ = test.o

# name of our output executable
OUT = test

# name of our logging header
TL_H = tl.h

# watched symbols file
EXPOSED_SYMS_FILE = .tlsyms
# TODO: header file with log_access functions for classes
EXPOSED_CLASSES_ACCESS_HEADER = tlclasses.h

# declare this first so make actually outputs executable correctly
$(OUT): $(OBJ)
	$(CC) -o $@ $^

clear_syms:
	rm -f ${EXPOSED_SYMS_FILE}

# create tl_%.c which processes all exposed variables, mutexes, etc
# TODO: should REMOVE all TL_EXPOSE_VAR on first pass to see if code even
# compiles; if it does compile, then insert our functions and do the real
# compilation; otherwise we totally clog up error messages with confusing crap
#
# CURRENTLY: tl.pl scans all code for such symbols and puts them all out to
# $(EXPOSED_SYMS_FILE), erroring out if it catches a repeat in names, and
# removes all instances of TL_EXPOSE_VAR and puts out to tl_%.c
tl_%.c: %.c clear_syms
	./tl.pl $< $@ $(EXPOSED_SYMS_FILE)

# after tl.pl scans everything for the exposed symbols, we want to make
# tl_repl_%.c which has the actual observer functions, and compile/link against
# those, which are the final versions of the code
# TODO: but first, we have to generate the log_access functions for classes from
# the .tlsyms file
tl_repl_%.c: tl_%.c $(EXPOSED_CLASSES_ACCESS_HEADER)
	./transform.pl $< $@ $(EXPOSED_SYMS_FILE)

# would be : $(EXPOSED_SYMS_FILE) but that's covered by the above rule
$(EXPOSED_CLASSES_ACCESS_HEADER):
	./insert_primitive_log_rw.pl $< $@

# create final object file (tl_%.c is removed by make)
$(OBJ): %.o: tl_repl_%.c $(DEPS) $(TL_H)
	$(CC) -c -o $@ $<

.PHONY: clean
clean:
	rm -f $(OUT)
	rm -f *.o
	rm -f tl_*.c
	rm -f $(EXPOSED_SYMS_FILE)
	rm -f ${EXPOSED_CLASSES_ACCESS_HEADER}
