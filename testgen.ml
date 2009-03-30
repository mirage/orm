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
