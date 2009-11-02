.DEFAULT: all

all:
	@ln -nsf ../../type-of/type.ml syntax/
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@

clean:
	$(MAKE) -C syntax $@
	$(MAKE) -f Makefile.orm $@
	rm -rf hash create type-of weakid
	rm -f syntax/type.ml

libinstall: all
	ocamlfind install orm META orm.cma orm.cmx orm.cmxa orm.a orm.cmi syntax/pa_orm.cma

libuninstall:
	ocamlfind remove orm
