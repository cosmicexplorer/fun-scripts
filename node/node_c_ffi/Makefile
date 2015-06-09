.PHONY: out npm_deps

CC = clang
COFFEE = coffee
CFLAGS = -std=c11 -Wall -Werror
LIB_COMPILE_FLAGS = -fpic
LIB_LINK_FLAGS = -shared
LFLAGS =
DEPS = mylib.h
OBJ = mylib.so
JS = run.js node-object-stream.js

%.o: %.c $(DEPS)
	$(CC) -c $(CFLAGS) $(LIB_COMPILE_FLAGS) $^
#	find . -name "*.gch" -exec rm '{}' ';'

%.so: %.o
	$(CC) $(LIB_LINK_FLAGS) $^ -o $@

%.js: %.coffee npm_deps
	$(COFFEE) -c $^

npm_deps:
	npm install

out: $(OBJ) $(JS)

.PHONY: clean
clean:
	rm -f $(OBJ)
	rm -f $(JS)
	rm -f $(wildcard *.gch)
