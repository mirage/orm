OCAMLMAKEFILE = OCamlMakefile

ANNOTATE = yes
export ANNOTATE
DEBUG = yes
export DEBUG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

SOURCES=printer_utils.ml sql_orm.ml
PACKS=unix sqlite3
RESULT=sql_orm
.PHONY: all
all: ncl bcl
	@ :

include $(OCAMLMAKEFILE)
