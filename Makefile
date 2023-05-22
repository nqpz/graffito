# Extend this list when you create new stencils.
STENCILS=gameoflife gameoflifeprob closingframe diamonds routefinder consistencyfier


.PHONY: all clean

MAKES=$(patsubst %,_make/%,$(STENCILS))
BINS=$(patsubst %,bin/%,$(STENCILS))
CLEANS=$(patsubst %,_clean/%,$(STENCILS))

all: $(MAKES) bin $(BINS)

_make/%:
	$(MAKE) -C stencils$(shell echo $@ | sed 's/^_make//')

bin:
	mkdir bin

bin/%:
	ln -s ../stencils$(shell echo $@ | sed 's/^bin//')/stencil $@

_clean/%:
	$(MAKE) clean -C stencils$(shell echo $@ | sed 's/^_clean//')

clean: $(CLEANS)
	rm -rf bin
