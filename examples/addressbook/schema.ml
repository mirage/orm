open Sql_orm.Schema

let all = make [
  "person" , [
    text ~flags:[`Unique] "name";
    integer ~flags:[`Optional] "age";
    text "email";
  ] ,
  [ 
    ["name"], ["age"]
  ],
  default_opts
] ;;

Sql_orm.generate ~debug:true all "my_db";;

