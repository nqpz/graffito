.PHONY: all clean

# FIXME: Make this Makefile smarter.
all:
	mkdir -p bin

	$(MAKE) -C stencils/gameoflife
	ln -sf ../stencils/gameoflife/stencil bin/gameoflife

	$(MAKE) -C stencils/closingframe
	ln -sf ../stencils/closingframe/stencil bin/closingframe

clean:
	$(MAKE) clean -C stencils/gameoflife
	$(MAKE) clean -C stencils/closingframe

	rm -r bin
