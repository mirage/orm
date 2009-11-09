OCAMLMAKEFILE = ../OCamlMakefile
ANNOTATE = yes

SOURCES = value.ml p4_value.ml pa_value.ml
PACKS = type-conv
RESULT = pa_value
USE_CAMLP4 = yes

.PHONY: all
all: dcl ncl
	@ :

install: all
	ocamlfind install value META value.cm{i,o,x} pa_value.cm{i,a} p4_value.cm{i,o}

uninstall:
	ocamlfind remove value

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(OCAMLMAKEFILE)
