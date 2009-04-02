(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Sql_orm.Schema

let all = make [
  "attachment" , [
    text "file_name";
    text "mime_type";
  ];

  "contact" , [
    text "first_name";
    text "last_name";
    text "email";
    date "mtime";
    foreign ~flags:[`Optional] "attachment" "image";
    foreign_many "attachment" "vcards";
    foreign_many "attachment" "notes";
  ];

  "entry" , [
    text "body";
    date "received";
    text ~flags:[`Optional] "subject";
    foreign "contact" "people_from";
    foreign_many "attachment" "atts";
    foreign_many "contact" "people_to";
  ];
]

let complex_foreign = make [
  "base" , [
    text "field1";
    date "date1";
    integer "int1";
  ];

  "middle" , [
    foreign "base" "f1";
    foreign "base" "f2";
    foreign_many "base" "f3";
    foreign_many "base" "f4";
  ];

  "last", [
    foreign "middle" "l1";
    foreign ~flags:[`Optional] "middle" "l2";
  ]
]

let _ = 
    Sql_orm.generate ~debug:false all "ormtest_normal";
    Sql_orm.generate ~debug:true  all "ormtest_debug";
    Sql_orm.generate ~debug:false complex_foreign "foreign_normal";
    Sql_orm.generate ~debug:true  complex_foreign "foreign_debug"
  
