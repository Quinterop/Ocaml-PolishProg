(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
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

(***********************************************************************)

let read_polish (filename:string) : program = failwith "TODO"

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
