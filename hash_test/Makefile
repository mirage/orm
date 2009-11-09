OCAMLMAKEFILE=../OCamlMakefile

.PHONY: run
run: dnc
	./$(RESULT) -verbose

.PHONY: slow
slow: dnc
	env SLOW=1 ./$(RESULT) -verbose

export INCDIRS = ../hash
export LIBDIRS = ../hash
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
