open Sql_orm

let all = Schema.make [
  "contact" , [
    Schema.text "first_name";
    Schema.text "last_name";
    Schema.text "email";
    Schema.date "mtime";
  ];

  "attachments" , [
    Schema.text "file_name";
    Schema.text "mime_type";
  ];

  "entry" , [
    Schema.text "body";
    Schema.date "received";
    Schema.foreign "people_from" "contact";
    Schema.foreign_many "atts" "attachments";
    Schema.foreign_many "people_to" "contact";
  ];

]

let _ = 
  generate all
