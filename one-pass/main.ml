let g exp = exp (fun m -> m)

(* main function *)
let go () =
  let var = Syntax.Var ("x") in
  let abst = Syntax.Abst ("x", var) in
  let app = Syntax.App (abst, var) in
  let sample = Syntax.Abst ("f",
      Syntax.Abst ("x",
      Syntax.Abst ("y",
        Syntax.App (Syntax.App (Syntax.Var ("f"), Syntax.Var ("y")),
                    Syntax.Var ("x")
                    )))) in
  print_string "Input : ";
  Syntax.print sample;
  print_string "Result : ";
  Syntax.print (g (Cps.f sample));	(* 結果を表示する *)
  print_newline ()
(* スタートアップ *)
let _ = go ()
