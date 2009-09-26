OCAMLMAKEFILE=../OCamlMakefile

.PHONY: run
run: dc
	./$(RESULT)

export INCDIRS = ../lib
export LIBDIRS = ../lib
export LIBS = orm
export PACKS = unix sqlite3 oUnit

RESULT= run_test
TESTS=  simple tuple variant alltypes foreign
SOURCES= test_utils.ml $(TESTS:%=%.ml) suite.ml

-include $(OCAMLMAKEFILE)
