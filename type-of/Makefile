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
	ocamlfind install type-of META type.cmi type.cmx type.cmo pa_type.cmi pa_type.cma

uninstall:
	ocamlfind remove type-of

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(OCAMLMAKEFILE)
