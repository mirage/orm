.DEFAULT: all

all:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

clean:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

install: all
	$(SUDO) ocamlfind install orm META orm.{a,cm{i,a,xa}} syntax/pa_orm.cma

uninstall:
	$(SUDO) ocamlfind remove orm

reinstall:
	$(MAKE) uninstall
	$(MAKE) install
