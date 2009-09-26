export OCAMLMAKEFILE = ../OCamlMakefile

export ANNOTATE = yes

define PROJ_sql
  SOURCES = weaktbl.ml sql_access.ml
  RESULT = sql_access
  export PACKS = sqlite3
endef
export PROJ_sql

define PROJ_orm
  SOURCES = sql_types.ml pa_sql_orm.ml
  PACKS = type-conv sqlite3
  RESULT = pa_sql_orm
  USE_CAMLP4 = yes
endef
export PROJ_orm

ifndef SUBPROJS
  export SUBPROJS = sql orm
endif

all:    dcl ncl

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
