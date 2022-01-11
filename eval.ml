
let var_table = Hashtbl.create 1234;;(*todo : gerer dynamiquement les tailles des tables.*)

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


(*FONCTIONS DE VARS POLISH*)
let allvars = Hashtbl.create 1234;;
let defvars = Hashtbl.create 1234;;
let defvarsif = Hashtbl.create 1234;;
let defvarselse = Hashtbl.create 1234;;

let set_vars s :unit =
  Hashtbl.replace allvars s 0;
  Hashtbl.replace defvars s 0;
;;

let set_varswhile s :unit =
  Hashtbl.replace allvars s 0;
;;

let set_varsif s :unit =
  Hashtbl.replace allvars s 0;
  Hashtbl.replace defvarsif s 0;
;;

let set_varselse s :unit =
  Hashtbl.replace allvars s 0;
  Hashtbl.replace defvarselse s 0;
;;

let intersect_tbl tb1 tb2 tbres =
Hashtbl.iter (fun a b ->if Hashtbl.find_opt tb2 a <> None then (Hashtbl.replace tbres a 0)) tb1
;;

let rec vars_expr exp =
  match exp with
  |Type.Num(n) -> ()
  |Type.Var(s)->if Hashtbl.find_opt allvars s = None then Hashtbl.replace allvars s 0;
  |Type.Op(op,exp,exp2)->vars_expr exp; vars_expr exp2 ;
;;

let vars_cond c =
  let (exp,comp,exp2) = c in
  vars_expr exp;vars_expr exp2;
;;

let vars_print e=
vars_expr e
;;

let rec vars_blockwhile p = (*le traitement des blocks if else et while est différent car il faut assigner leurs variables différemment.*)
  match p with
  |[]->();
  |a::y -> match a with
  |x,Type.Set (n,e)->vars_expr e;set_varswhile n ; vars_blockwhile y ;
  |x,Type.Read(n)->set_varswhile n  ; vars_blockwhile y;
  |x,Type.If(c,bl,bl2)->  vars_cond c;vars_if bl bl2; vars_blockwhile y ;
  |x,Type.Print(e)-> vars_print e ; vars_blockwhile y;
  |x,Type.While(c,b)-> vars_cond c ;vars_blockwhile b ;vars_blockwhile y ;
and vars_blockif p =
  match p with
  |[]->();
  |a::y -> match a with
  |x,Type.Set (n,e)->vars_expr e;set_varsif n ; vars_blockif y ;
  |x,Type.Read(n)->set_varsif n ; vars_blockif y;
  |x,Type.If(c,bl,bl2)->vars_cond c;vars_if bl bl2; vars_blockif y ;
  |x,Type.Print(e)-> vars_print e ; vars_blockif y;
  |x,Type.While(c,b)-> vars_cond c ;vars_blockwhile b ;vars_blockif y ;
and vars_if bl bl2 = 
  vars_blockif bl ;
  vars_blockelse bl2;
  intersect_tbl defvarsif defvarselse defvars;
  Hashtbl.clear defvarsif;     Hashtbl.clear defvarselse;
and vars_blockelse p =
  match p with
  |[]->();
  |a::y -> match a with
  |x,Type.Set (n,e)->vars_expr e;set_varselse n ; vars_blockelse y ;
  |x,Type.Read(n)->set_varselse n  ; vars_blockelse y;
  |x,Type.If(c,bl,bl2)-> vars_cond c;vars_if bl bl2; vars_blockelse y ;
  |x,Type.Print(e)-> vars_print e ; vars_blockelse y;
  |x,Type.While(c,b)-> vars_cond c ;vars_blockwhile b ;vars_blockelse y ;
;;

let difference_tbl tbl1 tbl2 difftb=
Hashtbl.iter (fun a b ->if Hashtbl.find_opt tbl2 a = None then (Hashtbl.replace difftb a 0)) tbl1
;;

let print_tbl tab= 
Hashtbl.iter (fun a b -> print_string (a^" ")) tab;
print_newline();
;;

let rec vars_polish p = 
  (* let x = Hashtbl.replace "abc" allvars in ()*)
  let rec vars_block p =
    match p with
    |[]->();
    |a::y -> match a with
    |x,Type.Set (n,e)->vars_expr e;set_vars n ; vars_block y 
    |x,Type.Read(n)->set_vars n  ; vars_block y 
    |x,Type.Print(e)-> vars_print e ; vars_block y ;
    |x,Type.If(c,bl,bl2)-> vars_cond c; let r = vars_if bl bl2 in () ; vars_block y ;
    |x,Type.While(c,b)-> vars_cond c ;vars_blockwhile b ; vars_block y ;
  in vars_block p;
  
  print_string "toutes variables : ";
  print_tbl allvars;
  let unvars = Hashtbl.create 1234 in 
  difference_tbl allvars defvars unvars;
  print_string "variables non def :";
  print_tbl unvars;
  print_string "variables definies  :";
  print_tbl defvars;
  ;;
