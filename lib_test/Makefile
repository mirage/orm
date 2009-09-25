OCAMLMAKEFILE=../OCamlMakefile

.PHONY: run
run: dc
	./$(RESULT)

export INCDIRS = ..
export LIBDIRS = ..
export LIBS = sql_access
export PACKS = unix sqlite3 oUnit

RESULT= run_test
TESTS=  simple tuple variant
SOURCES= test_utils.ml $(TESTS:%=%.ml) suite.ml

-include $(OCAMLMAKEFILE)
