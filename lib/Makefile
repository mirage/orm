.DEFAULT: all

all:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

clean:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

install: all
	ocamlfind install orm META orm.cma orm.cmx orm.cmxa orm.a orm.cmi syntax/pa_orm.cma

uninstall:
	ocamlfind remove orm

reinstall:
	$(MAKE) uninstall
	$(MAKE) install