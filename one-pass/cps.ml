open Syntax

(* CPS translate *)

(* Cps.f: Syntax.t -> Syntax.t *)
let rec f expr kappa = match expr with
  | Syntax.Var  (x)    -> kappa (Syntax.Var  (x))
  | Syntax.Abst (x, e) ->
    let var_k = Syntax.Var ("k") in
    kappa (Syntax.Abst (x,
                        Syntax.Abst ("k",
                                    (f e) (fun m -> Syntax.App (var_k, m)))))
  | Syntax.App  (e1, e2) ->
    let var_a = Syntax.Var ("a") in
    (f e1) (fun m -> (f e2)
                      (fun n ->
                          Syntax.App (Syntax.App (m, n), Syntax.Abst ("a", kappa var_a))
                      )
            )
