open Core.Std

open Ast

exception Unbound_variable of string
exception Unbound_function of string
exception Unimplemented of string

module Context = struct
  type t =
    { vars: Value.t String.Map.t;
      funcs: Funcdef.t String.Map.t
    }

  let add_var c ~key ~data = { c with vars = Map.add c.vars ~key ~data }

  let add_func c ~key ~data = { c with funcs = Map.add c.funcs ~key ~data }

  let find_var c = Map.find c.vars 

  let find_func c = Map.find c.funcs

  let empty = { vars = String.Map.empty; funcs = String.Map.empty }
end

let scoped_context ctx names vals =
  match List.zip names vals with
  | Some z ->
    let accumulator a (name, v) = Context.add_var a ~key:name ~data:v in
    let new_ctx = { ctx with Context.vars = String.Map.empty} in
    List.fold ~init:new_ctx ~f:accumulator z
  | None -> raise (Unimplemented "wrong number of params")

let rec eval_func ctx name eparams =
  let params = List.map eparams ~f:(fun p -> eval_expr ctx p) in
  match name with
  | "print" ->
    begin match List.nth params 0 with
      | None -> raise (Unbound_function "wrong number of params")
      | Some v ->
          let rec print_val = function
          | Value.Num n -> print_string (Number.string_of_number n)
          | Value.String s -> print_string s
          | Value.Bool b -> print_string (string_of_bool b)
          | Value.None -> print_string "None"
          | Value.List l -> 
            print_string "[" ; 
            List.iter ~f:(fun e -> print_val e; print_string ", ") l;
            print_string "]"
          in print_val v; print_endline ""; Value.None
    end

  | _ -> 
    begin match Context.find_func ctx name with
    | Some fdef ->
      let (param_names, statements) = (fdef.Funcdef.params, fdef.Funcdef.statements) in
      if (List.length param_names = List.length params)
      then 
        begin match snd (eval_chunk (scoped_context ctx param_names params) statements) with
        | Some retval -> retval
        | None -> Value.None
        end
      else raise (Unbound_function name)
    | None -> raise (Unbound_function name)
    end

and eval_expr ctx = 
  let eval_arith f e1 e2 =
    match (eval_expr ctx e1, eval_expr ctx e2) with
      | (Value.Num n1, Value.Num n2) -> Value.Num (f n1 n2)
      | _ -> raise (Failure "arithmetic on non-number") in
  let compare e1 e2 =
    let rec comp_vals = function
      | (Value.Num n1, Value.Num n2) -> n1 = n2
      | (Value.String s1, Value.String s2) -> s1 = s2
      | (Value.Bool b1, Value.Bool b2) -> b1 = b2
      | (Value.None, Value.None) -> true
      | (Value.List l1, Value.List l2) ->
        begin match List.zip l1 l2 with
        | None -> false
        | Some zipped ->  List.fold ~init:false ~f:(fun acc (a, b) -> if comp_vals (a, b) then acc else false) zipped
        end
      | _ -> raise (Failure "equality between different types")
    in
    comp_vals (eval_expr ctx e1, eval_expr ctx e2) in
  let comp_nums e1 e2 comp =
    match (eval_expr ctx e1, eval_expr ctx e2) with
    | (Value.Num n1, Value.Num n2) -> Value.Bool (comp n1 n2)
    | _ -> raise (Failure "Comparison must be between numbers") in
  function
  | Expr.Val v -> v
  | Expr.Var vc -> 
    begin match vc with
    | VarCall.Reg s ->
      begin match Context.find_var ctx s with
      | None -> raise (Unbound_variable s)
      | Some x -> x
      end
    | VarCall.List (s, ie) ->
      begin match Context.find_var ctx s with
        | None -> raise (Unbound_variable s)
        | Some x ->
          begin match x with
            | Value.List l ->
              begin match eval_expr ctx ie with
                | Value.Num n -> List.nth_exn l (Number.to_int n)
                | _ -> raise (Failure "index must be a number")
              end
            | _ -> raise (Failure "not a list")
          end
      end
    end
  | Expr.Funccall {Funccall.name = name; Funccall.params = params} -> 
    eval_func ctx name params
  | Expr.Plus (e1, e2) -> eval_arith Number.add e1 e2
  | Expr.Minus (e1, e2) -> eval_arith Number.sub e1 e2
  | Expr.Mult (e1, e2) -> eval_arith Number.mult e1 e2 
  | Expr.Div (e1, e2) -> eval_arith Number.div e1 e2
  | Expr.Equality (e1, e2) -> Value.Bool (compare e1 e2)
  | Expr.Inequality (e1, e2) -> Value.Bool (not (compare e1 e2))
  | Expr.GTEQ (e1, e2) -> comp_nums e1 e2 (>=)
  | Expr.LTEQ (e1, e2) -> comp_nums e1 e2 (<=)
  | Expr.GT (e1, e2) -> comp_nums e1 e2 (>)
  | Expr.LT (e1, e2) -> comp_nums e1 e2 (<)
  | Expr.ListInit (es) -> Value.List (List.map ~f:(eval_expr ctx) es)

and eval_statement ctx = function
  | Statement.Ifelse (expr, if_ss, else_ss_opt) ->
    begin match eval_expr ctx expr with
      | Value.Bool b -> 
        if b 
        then eval_chunk ctx if_ss 
        else 
          begin match else_ss_opt with
          | Some ss -> eval_chunk ctx ss
          | None -> (ctx, None)
          end
      | _ -> raise (Failure "If statement requires boolean expression")
    end
  | Statement.While (expr, chunk) ->
    begin match eval_expr ctx expr with
    | Value.Bool b ->
      if b
      then 
        begin match eval_chunk ctx chunk with
        | (new_ctx, None) -> eval_statement new_ctx (Statement.While (expr, chunk))
        | res -> res
        end 
      else (ctx, None)
    | _ -> raise (Failure "While statement requires boolean expression")
    end 
  | Statement.For (vname, e1, e2, ss) ->
    begin match (eval_expr ctx e1, eval_expr ctx e2) with
      | (Value.Num n1, Value.Num n2) ->
        if Number.lteq n1 n2
        then
          let incremented = Value.Num (Number.add n1 (Number.number_of_string "1")) in
          let for_scope = Context.add_var ~key:vname ~data:(Value.Num n1) in
          begin match eval_chunk (for_scope ctx) ss with
          | (new_ctx, None) -> 
            eval_statement (for_scope new_ctx) (Statement.For (vname, Expr.Val incremented, Expr.Val (Value.Num n2), ss))
          | res -> res
          end
        else (ctx, None)
    | (_, _) -> raise (Failure "For statement requires two numbers")
    end
  | Statement.Assign (vc, expr) ->
    begin match vc with
      | VarCall.Reg s -> (Context.add_var ctx ~key:s ~data:(eval_expr ctx expr), None)
      | VarCall.List (s, ie) ->
        begin match Context.find_var ctx s with
        | None -> raise (Unbound_variable "list doesn't exist")
        | Some v -> 
          begin match v with
            | Value.List l ->
              begin match eval_expr ctx ie with
                | Value.Num n ->
                  let index = Number.to_int n in
                  let nlist = 
                    List.mapi ~f:(fun i e -> if i = index then eval_expr ctx expr else e) l in
                  (Context.add_var ctx ~key:s ~data:(Value.List nlist), None)
                | _ -> raise (Failure "index must be number")
              end 
            | _ -> raise (Failure "list must be indexed")
          end
        end
    end
  | Statement.Funccall { Funccall.name = name; Funccall.params = exprs} -> 
    begin match name with
    | "append" -> 
      begin match (List.nth exprs 0, List.nth exprs 1) with
      | (None, _) -> raise (Unbound_function "wrong number of params")
      | (_, None) -> raise (Unbound_function "wrong number of params")
      | (Some e1, Some e2) ->
        begin match e1 with
        | Expr.Var vc ->
          begin match vc with
          | VarCall.Reg s -> 
            begin match Context.find_var ctx s with
            | Some (Value.List l) ->
              let nlist = l @ [eval_expr ctx e2] in
              (Context.add_var ctx ~key:s ~data:(Value.List nlist), None)
            | _ -> raise (Unbound_function "append only on list")
            end
          | VarCall.List _ -> raise (Unbound_function "append on list index not supported")
          end
        | _ -> raise (Unbound_function "append should be on list var")
        end 
      end
      | _ -> ignore (eval_func ctx name exprs); (ctx, None)
    end 
  | Statement.ListInit _ -> (ctx, None)
  | Statement.Return expr -> (ctx, Some (eval_expr ctx expr))

and eval_chunk ctx = function
  | st::tl ->
    begin match eval_statement ctx st with
    | (new_ctx, Some retval) -> (new_ctx, Some retval)
    | (new_ctx, None) -> eval_chunk new_ctx tl
    end
  | [] -> (ctx, None)

and eval_ast prog =
  let addfunc c f = Context.add_func c ~key:f.Funcdef.name ~data:f in
  let ctx = List.fold ~init:Context.empty ~f:(addfunc) prog.Program.funcs in
  ignore (eval_chunk ctx prog.Program.main)
