OCAMLMAKEFILE = ../../OCamlMakefile
ANNOTATE = yes

SOURCES = \
	../../hash/p4_hash.ml type.ml ../../type-of/p4_type.ml ../../value/p4_value.ml \
	../../weakid/p4_weakid.ml p4_utils.ml p4_orm.ml pa_orm.ml
PACKS = type-conv
RESULT = pa_orm
USE_CAMLP4 = yes

.PHONY: all
all: dcl ncl
	@ :

include $(OCAMLMAKEFILE)
