val gen1 :
  envfn:(string -> Camlp4.PreCast.Syntax.Ast.ctyp) ->
  Camlp4.PreCast.Syntax.Ast.ctyp -> Camlp4.PreCast.Syntax.Ast.expr
val gen :
  ?fun_name:(string -> string) ->
  Camlp4.PreCast.Syntax.Ast.ctyp -> Camlp4.PreCast.Syntax.Ast.str_item
