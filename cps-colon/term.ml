module rec Val : sig
  type t = VNum of int
         | VVar of string
         | VAbs of string * Term.t

  val to_str : t -> string
end = struct
  type t = VNum of int
         | VVar of string
         | VAbs of string * Term.t

  let to_str = function
    | VNum n -> string_of_int n
    | VVar v -> v
    | VAbs (x, e) -> "(λ" ^ x ^ ". " ^ Term.to_str e ^ ")"
end

and NVal : sig
  type t = App of Term.t * Term.t

  val to_str : t -> string
end = struct
  type t = App of Term.t * Term.t

  let to_str = function
    | App (e1, e2) -> "(" ^ Term.to_str e1 ^ " " ^ Term.to_str e2 ^ ")"
end

and Term : sig
  type t = Val of Val.t
         | NVal of NVal.t

  val to_str : t -> string
  val print  : t -> unit
end = struct
  type t = Val of Val.t
         | NVal of NVal.t

  let to_str = function
    | Val v  -> Val.to_str v
    | NVal e -> NVal.to_str e

  let print e =
    let str = to_str e in
    (print_string str; print_newline ())
end
