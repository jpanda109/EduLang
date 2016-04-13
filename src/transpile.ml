open Core.Std

open Ast

let rec eval_prog outc prog = 
  List.iter ~f:(ts_func outc) prog.Program.funcs;
  fprintf outc "\n"

and ts_func outc func = fprintf outc "function() {}"
