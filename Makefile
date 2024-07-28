# Extend this list when you create new stencils.
STENCILS=template steal gameoflife gameoflifeprob closingframe diamonds routefinder consistencyfier lines producerconsumer rain collatz


.PHONY: all clean

MAKES=$(patsubst %,_make/%,$(STENCILS))
BINS=$(patsubst %,bin/%,$(STENCILS))
CLEANS=$(patsubst %,_clean/%,$(STENCILS))

all: $(MAKES) bin $(BINS)

_make/%:
	$(MAKE) -C stencils$(shell echo $@ | sed 's/^_make//')

bin:
	mkdir -p bin

bin/%:
	ln -sf ../stencils$(shell echo $@ | sed 's/^bin//')/stencil $@

_clean/%:
	$(MAKE) clean -C stencils$(shell echo $@ | sed 's/^_clean//')

clean: $(CLEANS)
	rm -rf bin
