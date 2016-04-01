open Core.Std
open Lexing

open Lexer
open Ast

exception Unbound_variable of string
exception Unbound_function of string
exception Unimplemented of string

module Context = struct
  type t =
    { vars: Value.t String.Map.t;
      funcs: Funcdef.t String.Map.t
    }

  let add_var c ~key ~data = { c with vars = Map.add c.vars key data }

  let add_func c ~key ~data = { c with funcs = Map.add c.funcs key data }

  let find_var c = Map.find c.vars 

  let find_func c = Map.find c.funcs

  let empty = { vars = String.Map.empty; funcs = String.Map.empty }
end

let print_position outc lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outc "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Some (Parser.prog Lexer.read lexbuf) with
  | SyntaxError msg ->
    fprintf stdout "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stdout "%a: syntax error\n" print_position lexbuf;
    None

let scoped_context ctx names vals =
  match List.zip names vals with
  | Some z ->
    let accumulator a (name, v) = Context.add_var a name v in
    let new_ctx = { ctx with Context.vars = String.Map.empty} in
    List.fold ~init:new_ctx ~f:accumulator z
  | None -> raise (Unimplemented "wrong number of params")

let rec eval_func ctx name params =
  let params = List.map params ~f:(fun p -> eval_expr ctx p) in
  match name with
  | "print" ->
    begin match List.nth params 0 with
      | None -> raise (Unbound_function "wrong number of params")
      | Some v -> begin match v with
          | Value.Num n -> print_endline (string_of_int n); Value.None
          | Value.String s -> print_endline s; Value.None
          | Value.None -> print_endline "None"; Value.None
        end
    end
  | _ -> 
    begin match Context.find_func ctx name with
    | Some fdef ->
      let (param_names, statements) = (fdef.Funcdef.params, fdef.Funcdef.statements) in
      if (List.length param_names = List.length params)
      then eval_chunk (scoped_context ctx param_names params) statements
      else raise (Unbound_function name)
    | None -> raise (Unbound_function name)
    end

and eval_expr ctx = function
  | Expression.Val v -> v
  | Expression.Var s -> 
    begin match Context.find_var ctx s with
    | None -> raise (Unbound_variable s)
    | Some x -> x
    end
  | Expression.Funccall (name, params) -> eval_func ctx name params

and eval_statement ctx = function
  | Statement.Assign (s, expr) ->
    Context.add_var ctx ~key:s ~data:(eval_expr ctx expr)
  | Statement.If (expr, statements) ->
    raise (Unimplemented "If")
  | Statement.Ifelse (expr, if_st, else_st) ->
    raise (Unimplemented "Ifelse")
  | Statement.Funccall (name, exprs) -> ignore (eval_func ctx name exprs); ctx
  | Statement.Return expr -> raise (Unimplemented "Return")

and eval_chunk ctx = function
  | st::tl ->
    begin match st with
    | Statement.Return expr -> eval_expr ctx expr
    | _ -> eval_chunk (eval_statement ctx st) tl
    end
  | [] -> Value.None

and eval_ast ctx prog =
  ignore (eval_chunk ctx prog.Program.main)

let () =
  let contents = In_channel.read_all "test.edl" in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
    | Some ast -> eval_ast Context.empty ast
    | None -> print_endline("failure")
  end
