(* $Id: interp.ml,v 1.7 2019-01-29 17:26:15-08 - - $ *)
(* Parner: Samuel Guyette (sguyette@ucsc.edu)
   Partner: Sergey Gasparyan (sgaspary@ucsc.edu) *)
open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false


(* Evaulates a given express *)
let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Unary (oper, expr) ->
     (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr)
    | Binary (oper, expr1, expr2) ->
             (Hashtbl.find Tables.binary_fn_table oper)
                              (eval_expr expr1) (eval_expr expr2)
    | Memref memref -> match memref with
           | Variable ident -> Hashtbl.find Tables.variable_table ident
           | Arrayref (ident, exp) ->
                    (Hashtbl.find Tables.array_table
                         ident).(int_of_float((eval_expr exp)-. 1.0))

(* Interprets the print staatement *)
let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

(* Interprets the input statement *)
let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        try  let number = Etc.read_number ()
             in match memref with
                | Variable ident ->
                (Hashtbl.add Tables.variable_table  ident) number
                | Arrayref (ident, exp) ->
                (Hashtbl.find Tables.array_table ident).
                     (int_of_float((eval_expr exp) -. 1.0)) <- number
        with End_of_file ->
             (Hashtbl.add Tables.variable_table "eof" 1.0)
    in List.iter input_number memref_list

(* Interprets the dim statement *)
let interp_dim ident expr =
      (Hashtbl.add Tables.array_table ident)
           (Array.make (int_of_float (eval_expr expr)) 0.0)

(* Interprets the goto statement *)
let interp_goto label : Absyn.program option =
      Some ( Hashtbl.find Tables.label_table label)

(* Interprets the if statement *)
let interp_if expr label : Absyn.program option = match expr with
                     | Binary (oper, expr1, expr2) ->
                        if (Hashtbl.find Tables.compare_table oper)
                                (eval_expr expr1) (eval_expr expr2)
                                 then interp_goto label else None

(* Interprets the let statement *)
let interp_let memref expr =  match memref with
    | Variable ident ->
        (Hashtbl.add Tables.variable_table ident) (eval_expr expr)
    | Arrayref (ident, exp) -> (Hashtbl.find Tables.array_table ident).
        (int_of_float((eval_expr exp) -. 1.0)) <- (eval_expr expr)


let interp_stmt (stmt : Absyn.stmt) : Absyn.program option  =
    match stmt with
    | Dim (ident, expr) -> (interp_dim ident expr; None)
    | Let (memref, expr) -> (interp_let memref expr; None)
    | Goto label -> interp_goto label
    | If (expr, label) -> interp_if expr label
    | Print print_list -> (interp_print print_list; None)
    | Input memref_list -> (interp_input memref_list; None)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> match  (interp_stmt stmt) with
                             | None -> interpret otherlines
                             | Some label -> interpret label

let interpret_program program =
    (Tables.init_label_table program;
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
