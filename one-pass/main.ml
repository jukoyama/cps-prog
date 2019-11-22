let g exp = exp (fun m -> m)
let h exp = Syntax.Abst ("k", (exp (Syntax.Var ("k"))))

(* main function *)
let go () =
  let var = Syntax.Var ("x") in
  let abst = Syntax.Abst ("x", var) in
  let app = Syntax.App (abst, var) in
  let test = Syntax.Abst ("f",
      Syntax.Abst ("x",
      Syntax.Abst ("y",
        Syntax.App (Syntax.App (Syntax.Var ("f"), Syntax.Var ("y")),
                    Syntax.Var ("x")
                    )))) in

  let test_abst = Syntax.Abst ("f", Syntax.App (Syntax.Var ("f"), Syntax.Var ("x"))) in
  let test_abst_result1 = g (Cps.f test_abst) in
  let test_abst_result2 = g (Tail_calls.f test_abst) in
  print_string "Input : ";
  Syntax.print test_abst;
  print_string "Result : ";
  Syntax.print test_abst_result1;	(* 結果を表示する *)
  print_string "Result2 : ";
  Syntax.print test_abst_result2;
  print_newline ()
(* スタートアップ *)
let _ = go ()
