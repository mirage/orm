val hash_of : string -> string
val gen1 : envfn:(string -> Camlp4.PreCast.Syntax.Ast.ctyp option) ->Camlp4.PreCast.Syntax.Ast.ctyp -> Camlp4.PreCast.Syntax.Ast.expr
val gen : Camlp4.PreCast.Syntax.Ast.ctyp -> Camlp4.PreCast.Syntax.Ast.str_item
