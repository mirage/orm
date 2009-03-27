open Printf

module Printer = struct    
    type env = {
        fn: int -> string -> unit;
        p: string -> unit;         (* printer function *)
        i: int;                    (* indent level *)
        nl: unit -> unit;          (* new line *)
    }

    let indent e = { e with i = succ e.i; p = e.fn (succ e.i) }

    let indent_fn e fn =
        let e = indent e in
        fn e

    let list_iter_indent e fn l =
        List.iter (indent_fn e fn) l

    let hashtbl_iter_indent e fn h =
        Hashtbl.iter (indent_fn e fn) h

    let may fn = function
        |None -> ()
        |Some x -> fn x

    let must fn = function
        |None -> failwith "must"
        |Some x -> fn x

    let init_printer ?(msg=None) fout =
        let ind i s = String.make (i * 2) ' ' ^ s in
        let out i s = output_string fout ((ind i s) ^ "\n") in
        may (out 0) msg;
        {
            fn = out;
            i = 0;
            p = (out 0);
            nl = (fun (x:unit) -> out 0 "");
        }
        
    let print_module e n fn =
        e.p (sprintf "module %s = struct" (String.capitalize n));
        indent_fn e fn;
        e.p "end";
        e.nl ()

    let print_record e nm fn =
        e.p (sprintf "type %s = {" nm);
        indent_fn e fn;
        e.p "}";
        e.nl ()

    let print_object e nm fn =
        e.p (sprintf "type %s = <" nm);
        indent_fn e fn;
        e.p ">";
        e.nl ()

    let print_comment e x =
        e.p (sprintf "(* %s *)" x);
        
end

