open Sql_orm

let all = Schema.make [
  "attachment" , [
    Schema.text "file_name";
    Schema.text "mime_type";
  ];

  "contact" , [
    Schema.text "first_name";
    Schema.text "last_name";
    Schema.text "email";
    Schema.date "mtime";
    Schema.foreign ~flags:[`Optional] "attachment" "image";
    Schema.foreign_many "attachment" "vcards";
    Schema.foreign_many "attachment" "notes";
  ];

  "entry" , [
    Schema.text "body";
    Schema.date "received";
    Schema.text ~flags:[`Optional] "subject";
    Schema.foreign "contact" "people_from";
    Schema.foreign_many "attachment" "atts";
    Schema.foreign_many "contact" "people_to";
  ];
]

let _ = 
  generate ~debug:true all "ormtest"
