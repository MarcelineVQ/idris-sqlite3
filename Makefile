# literally just convenience

.PHONY: build

build:
	~/.idris2erl/bin/idris2erl --build package.ipkg

# install:
# 	idris2 --install package.ipkg

clean:
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
