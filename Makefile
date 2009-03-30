OCAMLMAKEFILE = OCamlMakefile

ANNOTATE = yes
export ANNOTATE
DEBUG = yes
export DEBUG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

DOC_FILES=sql_orm

SOURCES=printer_utils.ml sql_orm_header.ml sql_orm.ml
PACKS=unix sqlite3
RESULT=sql_orm

TRASH=sql_orm_header.ml

.PHONY: all
all: depend ncl bcl
	@ :

sql_orm_header.ml: sql_access.ml convert.ml
	ocaml convert.ml $< $@

.PHONY: depend
depend: sql_orm_header.ml
	@ :

.PHONY: install
install: depend libinstall
	@ :

.PHONY: uninstall
uninstall: libuninstall
	@ :	

include $(OCAMLMAKEFILE)
