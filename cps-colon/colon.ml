open Term
(* CPS translate *)

(* Colon.f: Term.t -> Value.t -> Term.t *)
let rec f e k =

  (* fv : Val.t -> Term.t *)
  let rec fv v = match v with
    | Val.VVar x      -> Term.Val v
    | Val.VNum n      -> Term.Val v
    | Val.VAbs (x, e) ->
      let var_k = Val.VVar "k" in
      Term.Val (Val.VAbs (x, Term.Val (Val.VAbs ("k", f e var_k))))
    | Val.VShf        ->
      let f_var x = Term.Val (Val.VVar x) in
      let id_x = Term.Val (Val.VAbs ("x", Term.Val (Val.VVar "x"))) in
      Term.Val (Val.VAbs ("w", Term.Val (Val.VAbs ("j",
        Term.NVal (NVal.App
          (Term.NVal (NVal.App (f_var "w",
             Term.Val (Val.VAbs ("y", Term.Val (Val.VAbs ("k",
               Term.NVal (NVal.App (f_var "k",
                 Term.NVal (NVal.App (f_var "j", f_var "y")))))))))), id_x))))))
  in

  (* fp : NVal.t -> Term.t *)
  let rec fp p k = match p with
    | NVal.Let (x, e1, e2) -> f e1 (Val.VAbs (x, f e2 k))
    | NVal.Rst (e)         -> Term.NVal (NVal.App (Term.Val k, f e (Val.VAbs ("x", Term.Val (Val.VVar "x")))))
    | NVal.App (e1, e2) ->
      match (e1, e2) with
      | (Term.NVal p1, Term.NVal p2) ->
        let var_x = Term.Val (Val.VVar "x") in
        let var_y = Term.Val (Val.VVar "y") in
        f e1
          (Val.VAbs
             ("x", f e2
                (Val.VAbs
                   ("y", Term.NVal (NVal.App (Term.NVal (NVal.App (var_x, var_y)), Term.Val k))))))
      | (Term.NVal p1, Term.Val  v2) ->
        let var_x = Term.Val (Val.VVar "x") in
        f e1
          (Val.VAbs
             ("x", Term.NVal (NVal.App (Term.NVal (NVal.App (var_x, fv v2)), Term.Val k))))
      | (Term.Val  v1, Term.NVal p2) ->
        let var_y = Term.Val (Val.VVar "y") in
        f e2
          (Val.VAbs
             ("y", Term.NVal (NVal.App (Term.NVal (NVal.App (fv v1, var_y)), Term.Val k))))
      | (Term.Val  v1, Term.Val  v2) ->
        Term.NVal (NVal.App (Term.NVal (NVal.App (fv v1, fv v2)), Term.Val k))
  in

  match e with
  | Term.Val v  -> Term.NVal (NVal.App (Term.Val k, fv v))
  | Term.NVal e -> fp e k
