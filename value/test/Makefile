OCAMLMAKEFILE=../../OCamlMakefile
ANNOTATE = yes

.PHONY: run
run: dnc
	./$(RESULT) -verbose

.PHONY: slow
slow: dnc
	env SLOW=1 ./$(RESULT) -verbose

export INCDIRS = ..
export LIBDIRS = ..
export LIBS = pa_value
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
