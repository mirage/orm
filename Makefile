OCAMLMAKEFILE = OCamlMakefile

ANNOTATE = yes
export ANNOTATE
DEBUG = yes
export DEBUG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

SOURCES=printer_utils.ml sql_orm.ml
THREADS=yes
RESULT=ormgen
PACKS=unix sqlite3
LIBDIRS=/opt/local/lib

PP=./camlp4find $(PACKS)
export PP

.PHONY: all
all: dnc
	@ :

run:
	./$(RESULT) > ormtest.ml

include $(OCAMLMAKEFILE)
