type position = int
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

let rec print_expr exp=
  match exp with
  |Num(n) ->print_int n
  |Var(s)->print_string s 
  |Op(op,exp,exp2)->print_op op exp exp2 ;
  and 
  print_op op exp exp2 = 
  match op with
  |Sub -> print_string "- "; print_expr exp;  print_string " "; print_expr exp2
  |Mul -> print_string "* "; print_expr exp;  print_string " "; print_expr exp2
  |Div -> print_string "/ "; print_expr exp;  print_string " "; print_expr exp2
  |Add -> print_string "+ "; print_expr exp;  print_string " "; print_expr exp2
  |Mod -> print_string "% "; print_expr exp; print_string " "; print_expr exp2
  ;;
  
  let print_comp comp expr1 expr2 = 
    match comp with 
    | Eq -> print_expr expr1 ;print_string " = ";print_expr expr2
    | Ne -> print_expr expr1 ;print_string " <> " ;print_expr expr2 
    | Lt -> print_expr expr1 ;print_string " < ";print_expr expr2 
    | Le -> print_expr expr1 ;print_string " <= " ;print_expr expr2 
    | Gt -> print_expr expr1 ;print_string " > ";print_expr expr2 
    | Ge -> print_expr expr1 ;print_string " >= " ;print_expr expr2 
  ;;
  let print_cond c :unit=
  let (exp,comp,exp2) = c in 
  print_comp comp exp exp2
  ;;

  (*FONCTIONS DE PRINT POLISH*)
  let rec print_expr exp=
  match exp with
  |Num(n) ->print_int n
  |Var(s)->print_string s 
  |Op(op,exp,exp2)->print_op op exp exp2 ;
  and 
  print_op op exp exp2 = 
  match op with
  |Sub -> print_string "- "; print_expr exp;  print_string " "; print_expr exp2
  |Mul -> print_string "* "; print_expr exp;  print_string " "; print_expr exp2
  |Div -> print_string "/ "; print_expr exp;  print_string " "; print_expr exp2
  |Add -> print_string "+ "; print_expr exp;  print_string " "; print_expr exp2
  |Mod -> print_string "% "; print_expr exp; print_string " "; print_expr exp2
  ;;
  
  let print_comp comp expr1 expr2 = 
    match comp with 
    | Eq -> print_expr expr1 ;print_string " = ";print_expr expr2
    | Ne -> print_expr expr1 ;print_string " <> " ;print_expr expr2 
    | Lt -> print_expr expr1 ;print_string " < ";print_expr expr2 
    | Le -> print_expr expr1 ;print_string " <= " ;print_expr expr2 
    | Gt -> print_expr expr1 ;print_string " > ";print_expr expr2 
    | Ge -> print_expr expr1 ;print_string " >= " ;print_expr expr2 
  ;;
  let print_cond c :unit=
  let (exp,comp,exp2) = c in 
  print_comp comp exp exp2
  ;;
  
  let check_empty_block b =
    match b with
    |[] ->true;
    |_-> false
  ;;
  
  let print_polish (p:program) : unit =
    let rec print_block p =
      match p with
      |[]->();
      |a::y -> match a with
      |_,Set (n,e)->print_string (n ^" := "); (print_expr e) ;print_newline(); print_block y ;
      |_,Read(n)->print_string("READ " ^ n);print_newline(); print_block y;
      |_,If(c,bl,bl2)-> print_string "IF "; print_cond c ; print_newline(); print_block bl ;if not (check_empty_block bl2) then( print_string "ELSE"; print_newline(); print_block bl2); print_block y ;
      |_,Print(e)-> print_string "PRINT "; print_expr e ;print_newline(); print_block y;
      |_,While(c,b)->print_string "WHILE "; print_cond c; print_newline(); print_block b ; print_block y ;
    in print_block p
  ;;