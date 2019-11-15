(* Syntax.t: abstract-syntax construct *)

type t = Var of string        (* indentifier *)
       | Abst of string * t   (* lambda abstract *)
       | App of t * t (* application *)

(* Syntax.print: print abstract-syntax construct  *)
let rec to_string expr = match expr with
  | Var (name) -> name
  | Abst (name, args) ->
    "( λ" ^ name ^ ". " ^ to_string args ^ " )"
  | App (arg1, arg2) ->
	  "( @ " ^ to_string arg1 ^ " " ^ to_string arg2 ^ " )"

let print expr =
  let str = to_string expr
  in (print_string str;
      print_newline ())
