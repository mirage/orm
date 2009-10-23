OCAMLMAKEFILE=../OCamlMakefile

.PHONY: run
run: dc
	./$(RESULT) -verbose

export INCDIRS = ../lib
export LIBDIRS = ../lib
export LIBS = orm
export PACKS = unix sqlite3 ounit

RESULT= run_test
TESTS=  simple tuple variant alltypes foreign recursive array_simple \
	foreign_and_variant foreign_tuple list_foreign list_simple \
	list_tuple list_list nested_tuple nested_option variant_nested
 
SOURCES= test_utils.ml $(TESTS:%=%.ml) suite.ml

pc_%:
	$(MAKE) -f Makefile.debug $@

p_%:
	$(MAKE) -f Makefile.debug $@

pi_%:
	$(MAKE) -f Makefile.debug $@

i_%:
	$(MAKE) -f Makefile.debug $@

-include $(OCAMLMAKEFILE)
