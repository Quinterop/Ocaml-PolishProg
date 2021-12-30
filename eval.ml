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
  eval_op op exp exp2 :position=
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