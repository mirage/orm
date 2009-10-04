.DEFAULT: all

all:
	$(MAKE) -f Makefile.syntax $@
	$(MAKE) -f Makefile.orm $@

clean:
	$(MAKE) -f Makefile.syntax $@
	$(MAKE) -f Makefile.orm $@

libinstall: all
	ocamlfind install orm META orm.cma orm.cmx orm.cmxa orm.a orm.cmi pa_orm.cma
