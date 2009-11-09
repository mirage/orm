OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE = yes

SOURCES = p4_hash.ml pa_hash.ml
PACKS = type-conv
RESULT = pa_hash
USE_CAMLP4 = yes

.PHONY: all
all: dcl
	@ :

install: all
	ocamlfind install hash META pa_hash.cma pa_hash.cmi

uninstall:
	ocamlfind remove hash

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(OCAMLMAKEFILE)
