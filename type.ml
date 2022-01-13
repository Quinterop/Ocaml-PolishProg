(** Projet Polish -- Analyse statique d'un mini-langage impératif *)


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

let fact :program = [
(1, Read("g")); 
(2, Read("n"));
(3, Set("i", Num(1)));
(4, Set("r", Num(1)));
(5, While(
  (Var("i"),Le,Var("n")),
  [
    (1,Print(Var("i")));
    (2,Print(Var("r")));
    (3,Set("r", Op(Mul, Var("i"), Var("r"))));
    (4,Set("i", Op(Add, Var("i"), Num(1))));
  ]
));
(6, Print(Var("r")));
(7,If(
  (Num(1),Eq,Num(1)),
  [
    (1, Set("a", Num(1)));
    (2, Print(Var("a")));
  ],
  [
    (1, Print(Var("n")));
  ]
  ));
]

let test :program = [
  (1, Set("a", Num(1)));
  (2, Set("b", Num(2)));
  (3,If(
    (Num(1),Ge,Num(0)),
    [
      (1, Print(Var("b")));
      (1, Print(Var("a")));
      (1, Print(Var("b")));

    ],
    [
(1,Set("b", Op(Add, Op(Add, Var("a"), Num(0)), Op(Add, Var("a"), Num(0)))));
      ]));]