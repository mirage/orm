.DEFAULT: all

all:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

clean:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

install: all
	$(SUDO) $(MAKE) -f Makefile.orm libinstall

uninstall:
	$(SUDO) ocamlfind remove orm

reinstall:
	$(MAKE) uninstall
	$(MAKE) install
