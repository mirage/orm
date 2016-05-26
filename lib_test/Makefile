OCAMLMAKEFILE=../OCamlMakefile
ANNOTATE=yes

.PHONY: run
run: dnc
	./$(RESULT) -verbose

.PHONY: slow
slow: dnc
	env SLOW=1 ./$(RESULT) -verbose

export INCDIRS = ../lib
export LIBDIRS = ../lib
export LIBS = orm
export CLIBS = sqlite3
export PACKS = unix sqlite3 oUnit dyntype num
export PPFLAGS = $(shell ocamlfind query dyntype.syntax -predicates \
		   syntax,preprocessor -format "-I %d %a" -r) \
		 -I ../lib/syntax pa_orm.cma

RESULT= run_test
TESTS=  simple tuple variant alltypes foreign recursive array_simple \
	foreign_and_variant foreign_tuple list_foreign list_simple \
	list_tuple list_list nested_tuple nested_option variant_nested \
	stress record_mutate list_mutate recursive_mutate photo object_simple \
	bibtex bib large_string hash get_set list_share delete big_list stress_mutate \
	option_rec

TRASH= *.db
SOURCES= test_utils.ml $(TESTS:%=%.ml) suite.ml
ANNOTATE=yes

pc_%:
	$(MAKE) -f Makefile.debug $@

p_%:
	$(MAKE) -f Makefile.debug $@

pi_%:
	$(MAKE) -f Makefile.debug $@

i_%:
	$(MAKE) -f Makefile.debug $@

-include $(OCAMLMAKEFILE)
