OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE = yes

SOURCES = p4_weakid.ml pa_weakid.ml hweak.mli hweak.ml
PACKS = type-conv hash.syntax
RESULT = pa_weakid
USE_CAMLP4 = yes

.PHONY: all
all: dcl
	@ :

install: all
	ocamlfind install weakid META pa_weakid.cm{i,a} p4_weakid.cm{i,o} hweak.cm{i,o}

uninstall:
	ocamlfind remove weakid

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(OCAMLMAKEFILE)
