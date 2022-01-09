
let var_table = Hashtbl.create 123456;;
let eval_read n =
  print_string "assigner la variable ";
  print_string n;
  print_newline();
  let input:int = read_int() in
  Hashtbl.add var_table n input
;;

let rec eval_expr exp =
  match exp with
  |Type.Num(n) -> n
  |Type.Var(s)->Hashtbl.find var_table s
  |Type.Op(op,exp,exp2)->eval_op op exp exp2
  and
  eval_op op exp exp2 :Type.position=
  match op with
  |Type.Add -> eval_expr exp + eval_expr exp2
  |Type.Sub -> eval_expr exp - eval_expr exp2
  |Type.Mul -> eval_expr exp * eval_expr exp2
  |Type.Div -> eval_expr exp / eval_expr exp2
  |Type.Mod -> eval_expr exp  mod eval_expr exp2
;;

let eval_print e =
  print_int (eval_expr e);
  print_newline ();
;;

let eval_comp comp expr1 expr2 =
  match comp with
  | Type.Eq -> eval_expr expr1 = eval_expr expr2
  | Type.Ne -> eval_expr expr1 <> eval_expr expr2
  | Type.Lt -> eval_expr expr1 < eval_expr expr2
  | Type.Le -> eval_expr expr1 <= eval_expr expr2
  | Type.Gt -> eval_expr expr1 > eval_expr expr2
  | Type.Ge -> eval_expr expr1 >= eval_expr expr2
;;

let eval_cond c =
  let (exp,comp,exp2) = c in
  (eval_comp comp exp exp2)
;;

let eval_set i s :unit =
  Hashtbl.add var_table s i
;;

let eval_polish (p:Type.program) : unit =
  let rec eval_block p =
    match p with
    |[]->();
    |a::y -> match a with
    |x,Type.Set (n,e)->eval_set (eval_expr e) n ; eval_block y ; print_int x;
    |x,Type.Read(n)->eval_read n  ; eval_block y; print_int x;
    |x,Type.If(c,bl,bl2)-> if eval_cond c then eval_block bl else eval_block bl2 ; eval_block y ; print_int x;
    |x,Type.Print(e)-> eval_print e ; eval_block y; print_int x;
    |x,Type.While(c,b)->if eval_cond c then ( eval_block b ; eval_block p) else eval_block y ; print_int x;
  in eval_block p
;;