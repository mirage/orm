OCAMLMAKEFILE = ../OCamlMakefile

ANNOTATE = yes
export ANNOTATE

SOURCES=test_schema.ml
RESULT=test_schema_generate
PACKS=unix sql_orm
TRASH=ormtest.ml ormtest.mli 

.PHONY: all
all: dnc
	./$(RESULT)

include $(OCAMLMAKEFILE)

