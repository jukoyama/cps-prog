open Term
(* let g exp = exp (fun m -> m)
 * let h exp = Syntax.Abst ("k", (exp (Syntax.Var ("k")))) *)

(* main function *)
let go () =

  let h e = Term.Val (Val.VAbs ("c", e)) in

  let varx = Term.Val (Val.VVar ("x")) in
  let con = Val.VVar ("c") in

  let fn n = Term.Val (Val.VNum n) in
  let e1 = Term.Val (Val.VAbs ("x", Term.Val (Val.VAbs ("y", varx)))) in
  let e = Term.NVal (NVal.App (Term.NVal (NVal.App (e1, fn 2)), fn 1)) in

  let test = h (Colon.f e con) in
  
  print_string "Input : ";
  Term.print e;
  print_string "Result : ";
  Term.print test;	(* 結果を表示する *)
  print_newline ()
(* スタートアップ *)
let _ = go ()
