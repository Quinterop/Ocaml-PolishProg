type position = int ;;
(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
| Num of int
| Var of name
| Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
| Set of name * expr
| Read of name
| Print of expr
| If of cond * block * block
| While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)

type program = block

let abs:program = [
(1, Read("n"));
(2, If(
(Var("n"), Lt, Num(0)),
[
(1, Set("res", Op(Sub, Num(0), Var("n"))));
],
[
(1, Set("res", Var("n")));
]
));
(3, Print(Var("res")));
]


let factors:program = [
(1, Read("n"));
(2, If(
(Var("n"), Le, Num(0)),
[
(1, Print(Num(-1)));
],
[
(1, Set("i", Num(2)));
(2, While(
(Op(Mul, Var("i"), Var("i")), Le, Var("n")),
[
(1, If(
(Op(Mod, Var("n"), Var("i")), Eq, Num(0)),
[
(1, Print(Var("i")));
(2, Set("n", Op(Div, Var("n"), Var("i"))));
],
[
(1, Set("i", Op(Add, Var("i"), Num(1))));
]
));
]
));
(3, Print(Var("n")));
]
));
]

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
  |Num(n) -> n
  |Var(s)->Hashtbl.find var_table s
  |Op(op,exp,exp2)->eval_op op exp exp2
  and
  eval_op op exp exp2 : position=
  match op with
  |Add -> eval_expr exp + eval_expr exp2
  |Sub -> eval_expr exp - eval_expr exp2
  |Mul -> eval_expr exp * eval_expr exp2
  |Div -> eval_expr exp / eval_expr exp2
  |Mod -> eval_expr exp  mod eval_expr exp2
;;

let eval_print e =
  print_int (eval_expr e);
  print_newline ();
;;

let eval_comp comp expr1 expr2 =
  match comp with
  | Eq -> eval_expr expr1 = eval_expr expr2
  | Ne -> eval_expr expr1 <> eval_expr expr2
  | Lt -> eval_expr expr1 < eval_expr expr2
  | Le -> eval_expr expr1 <= eval_expr expr2
  | Gt -> eval_expr expr1 > eval_expr expr2
  | Ge -> eval_expr expr1 >= eval_expr expr2
;;

let eval_cond c =
  let (exp,comp,exp2) = c in
  (eval_comp comp exp exp2)
;;

let eval_set i s :unit =
  Hashtbl.add var_table s i
;;

let eval_polish (p:program) : unit =
  let rec eval_block p =
    match p with
    |[]->();
    |a::y -> match a with
    |x,Set (n,e)->eval_set (eval_expr e) n ; eval_block y ; print_int x;
    |x,Read(n)->eval_read n  ; eval_block y; print_int x;
    |x,If(c,bl,bl2)-> if eval_cond c then eval_block bl else eval_block bl2 ; eval_block y ; print_int x;
    |x,Print(e)-> eval_print e ; eval_block y; print_int x;
    |x,While(c,b)->if eval_cond c then ( eval_block b ; eval_block p) else eval_block y ; print_int x;
  in eval_block p
;;

(*FONCTIONS DE VARS POLISH*)
module Vars = Set.Make(String)

let allvars = Vars.empty
let defvars = Vars.empty
let defvarsif = Vars.empty
let defvarselse = Vars.empty
let set_vars s :unit =
  let x = Vars.add s allvars in ();
  let y = Vars.add s defvars in ()
;;
let set_varswhile s :unit =
  let x = Vars.add s allvars in ();
;;
let set_varsif s :unit =
  let x = Vars.add s allvars in ();
  let y = Vars.add s defvarsif in ()
;;
let set_varselse s :unit =
  
  let x = Vars.add s allvars in ();
  let y = Vars.add s defvarselse in ();
;;

(*let rec vars_expr exp =
match exp with
|Num(n) -> ()
|Var(s)->if Vars.find_opt s allvars = None then let x = Vars.add s allvars in ();
|Op(op,exp,exp2)->eval_op op exp exp2 ;
  and
  vars_op op exp exp2 =
  match op with
  |Add -> let x = vars_expr exp + vars_expr exp2  in ();
  |Sub -> let x = vars_expr exp - vars_expr exp2 in ();
  |Mul -> let x = vars_expr exp * vars_expr exp2 in ();
  |Div -> let x = vars_expr exp / vars_expr exp2 in ();
  |Mod -> let x = vars_expr exp  mod vars_expr exp2 in ();
  ;;
*)



let vars_comp comp expr1 expr2 =
match comp with
| Eq -> let x = vars_expr expr1 = vars_expr expr2 in()
| Ne -> let x = vars_expr expr1 <> vars_expr expr2 in()
| Lt -> let x = vars_expr expr1 < vars_expr expr2 in()
| Le -> let x = vars_expr expr1 <= vars_expr expr2 in()
| Gt -> let x = vars_expr expr1 > vars_expr expr2 in()
| Ge -> let x = vars_expr expr1 >= vars_expr expr2 in()
;;

let vars_cond c =
let (exp,comp,exp2) = c in
(vars_comp comp exp exp2)
;;

let vars_print e=
vars_expr e
;;


let rec vars_blockwhile p =
  match p with
  |[]->();
  |a::y -> match a with
  |x,Set (n,e)->vars_expr e;set_varswhile n ; vars_blockwhile y ;
  |(x,Read(n))->set_varswhile n  ; vars_blockwhile y;
  |x,If(c,bl,bl2)->  vars_cond c; vars_blockwhile y ;
  |x,Print(e)-> vars_print e ; vars_blockwhile y;
  |x,While(c,b)-> vars_cond c ;vars_blockwhile b ;vars_blockwhile y ;
;;let rec vars_blockif p =
  match p with
  |[]->();
  |a::y -> match a with
  |x,Set (n,e)->vars_expr e;set_varsif n ; vars_blockif y ;
  |(x,Read(n))->set_varsif n  ; vars_blockif y;
  |x,If(c,bl,bl2)->  vars_cond c; vars_blockif y ;
  |x,Print(e)-> vars_print e ; vars_blockif y;
  |x,While(c,b)-> vars_cond c ;vars_blockif b ;vars_blockif y ;
;;
let rec vars_blockelse p =
  match p with
  |[]->();
  |a::y -> match a with
  |x,Set (n,e)->vars_expr e;set_varswhile n ; vars_blockwhile y ;
  |(x,Read(n))->set_varswhile n  ; vars_blockwhile y;
  |x,If(c,bl,bl2)->  vars_cond c; vars_blockwhile y ;
  |x,Print(e)-> vars_print e ; vars_blockwhile y;
  |x,While(c,b)-> vars_cond c ;vars_blockwhile b ;vars_blockwhile y ;
;;

let rec loop list =
  match list with 
  |a::d'-> let x = Vars.add a defvars in (); loop d';
  |[]->();
;;

let rec looprint list =
  match list with 
  |a::d'-> print_string a; looprint d';
  |[]->print_newline();
;;
let vars_if bl bl2 = 
  vars_blockif bl ;print_string "blockif ok";
  vars_blockelse bl2;print_string "blockelse ok";
  let union = Vars.union defvarsif defvarselse in 
  let pattern = Vars.elements union in
  loop pattern
;;

let rec vars_polish p = 
  let rec vars_block p =
    match p with
    |[]->();
    |a::y -> match a with
    |x,Set (n,e)->vars_expr e;set_vars n ;print_string "set ok"; vars_block y 
    |(x,Read(n))->set_vars n  ;print_string "read ok"; vars_block y 
    |x,If(c,bl,bl2)-> vars_cond c; vars_if bl bl2 ;print_string "if ok"; vars_block y 
    |x,Print(e)-> vars_print e ; print_string "print ok"; vars_block y 
    |x,While(c,b)-> vars_cond c ;vars_blockwhile b ;print_string "while ok"; vars_block y 
  in vars_block p;
  let all = Vars.elements allvars in looprint all;
  let not = Vars.elements(Vars.diff allvars defvars) in looprint not;
;;