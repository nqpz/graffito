# Extend this list when you create new stencils.
STENCILS=template steal gameoflife gameoflifeprob closingframe diamonds routefinder consistencyfier lines producerconsumer rain collatz gravity squarespiral coralreef


.PHONY: all clean sync

MAKES=$(patsubst %,_make/%,$(STENCILS))
BINS=$(patsubst %,bin/%,$(STENCILS))
CLEANS=$(patsubst %,_clean/%,$(STENCILS))

all: $(MAKES) bin $(BINS)

$(MAKES): sync

sync:
	@if [ ! -d lib ]; then \
		echo "Running futhark pkg sync..."; \
		futhark pkg sync; \
	fi

_make/%:
	$(MAKE) -C stencils$(shell echo $@ | sed 's/^_make//')

bin:
	mkdir -p bin

bin/%:
	ln -sf ../stencils$(shell echo $@ | sed 's/^bin//')/stencil $@

_clean/%:
	$(MAKE) clean -C stencils$(shell echo $@ | sed 's/^_clean//')

clean: $(CLEANS)
	rm -rf bin *.lyscache
