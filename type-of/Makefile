OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE = yes

SOURCES = type.ml p4_type.ml pa_type.ml
PACKS = type-conv
RESULT = pa_type
USE_CAMLP4 = yes

.PHONY: all
all: dcl ncl
	@ :

install: all
	ocamlfind install type-of META type.{o,cm{i,o,x}} pa_type.cm{i,a} p4_type.cm{i,o}

uninstall:
	ocamlfind remove type-of

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(OCAMLMAKEFILE)
