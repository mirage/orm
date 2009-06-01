all: test.opt
	./$<

OCAMLFIND=ocamlfind ocamlc -thread -package sql_orm 
COMPILE=$(OCAMLFIND) -c -g
LINK=$(OCAMLFIND) -linkpkg -g -o

schema.cmo schema.cmi: schema.ml
	$(COMPILE) $^

my_db.cmo my_db.cmi: my_db.mli my_db.ml
	$(COMPILE) $^

test.cmo: test.ml
	$(COMPILE) $^

test.opt: my_db.cmo test.cmo
	$(OCAMLFIND) -linkpkg -g -o $@ $^

schema.opt: schema.cmo
	$(OCAMLFIND) -linkpkg -g -o $@ $^

my_db.ml my_db.mli: schema.opt
	./$<

clean:
	rm -f *.cmi *.cmo *.opt *.annot test.db

.INTERMEDIATE: schema.cmo schema.cmi my_db.cmo my_db.cmi test.cmo test.cmi
.PHONY: all clean
.DEFAULT: all
