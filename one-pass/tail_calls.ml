open Syntax

(* One-pass, "properly tail-calls" CPS transformation *)

(* Tail-calls.f: Syntax.t -> Syntax.t *)
let rec f expr kappa =

  let rec g expr k = match expr with
    | Syntax.Var  (x)      -> Syntax.App (k, expr)
    | Syntax.Abst (x, e)   ->
      Syntax.App (k,
                  Syntax.Abst (x,
                               Syntax.Abst ("k",
                                            (g e) (Syntax.Var ("k"))
                                            )
                               )
                  )
    | Syntax.App  (e1, e2) ->
      (f e1) (fun m -> (f e2)
                        (fun n -> Syntax.App (Syntax.App (m, n), k)))
  in

  match expr with
  | Syntax.Var  (x)    -> kappa expr
  | Syntax.Abst (x, e) ->
    let var_k = Syntax.Var ("k") in
    kappa (Syntax.Abst (x,
                        Syntax.Abst ("k",
                                    (g e) (Syntax.Var ("k")))))
  | Syntax.App  (e1, e2) ->
    let var_a = Syntax.Var ("a") in
    (f e1) (fun m -> (f e2)
                      (fun n ->
                          Syntax.App (Syntax.App (m, n), Syntax.Abst ("a", kappa var_a))
                      )
            )
