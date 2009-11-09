OCAMLMAKEFILE=../../OCamlMakefile

.PHONY: run
run: dnc
	./$(RESULT) -verbose

.PHONY: slow
slow: dnc
	env SLOW=1 ./$(RESULT) -verbose

export INCDIRS =
export LIBDIRS =
export PACKS = unix oUnit weakid.syntax
export PREDS = camlp4o

RESULT = run_test
TESTS  = all


SOURCES= $(TESTS:%=%.ml) suite.ml

pc_%:
	$(MAKE) -f Makefile.debug $@

p_%:
	$(MAKE) -f Makefile.debug $@

pi_%:
	$(MAKE) -f Makefile.debug $@

i_%:
	$(MAKE) -f Makefile.debug $@

-include $(OCAMLMAKEFILE)
