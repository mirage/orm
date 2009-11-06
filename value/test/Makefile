OCAMLMAKEFILE=../../OCamlMakefile
ANNOTATE = yes

.PHONY: run
run: dnc
	./$(RESULT) -verbose

.PHONY: slow
slow: dnc
	env SLOW=1 ./$(RESULT) -verbose

export INCDIRS = .. ../../hash ../../weakid
export LIBDIRS = .. ../../hash ../../weakid
export LIBS = pa_hash pa_weakid pa_value
export PACKS = unix oUnit

RESULT= run_test
TESTS=  all

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
