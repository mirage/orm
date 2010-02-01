OCAMLMAKEFILE = ../OCamlMakefile

ANNOTATE = yes
SOURCES = weakid.mli weakid.ml
RESULT = weakid

.PHONY: all
all: dcl nc
	 @ :

install: all
	$(SUDO) ocamlfind install weakid META weakid.{o,cm{i,o,x}}

uninstall:
	$(SUDO) ocamlfind remove weakid

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

include $(OCAMLMAKEFILE)
