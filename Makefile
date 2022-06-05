# literally just convenience

.PHONY: build

build:
	@sae build

support:
	@c99 -lsqlite3 -shared idris_sqlite.c -o idris_sqlite.so

repl:
	@rlwrap sae repl

# install:
#   idris2 --install package.ipkg

clean:
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
