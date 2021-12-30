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
let read_polish (filename:string) : program = failwith "TODO"
(*
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

*)
(***********************************************************************)
(* FONCTIONS DE READ POLISH*)
(*découpe les fichiers en lignes et les met dans une int * string list. 
let rec add_lines input (no:int) lines  =
try
  add_lines(input)(no+1) (List.cons (no,(input_line input))lines)
with End_of_file ->close_in input;lines
;;

let line_parser filename =
  try
    let ic = open_in filename in 
    let (lines: (int * string) list) = [] in
    List.rev (add_lines ic 1 lines )
  with e ->failwith "fichier non ouvrable"
;;
let word_cutter (line:string) :string list =  
  String.split_on_char ' ' line
;;

(*prend un string et renvoie un int correspondant aux espaces avant le premier autre caractere*)
let tabify s = List.init (String.length s) (String.get s)
;;
let indentation s= 
let t = tabify s in
let rec indent_aux t n =
  match t with
  | [] -> n
  |c::t'-> if c =' ' then indent_aux (t') (n+1) else n in 
  indent_aux t 0
;;
let get_line lines no = 
  List.assoc no lines 
;;
(*
let read_polish (filename:string) : program = 
  let file = line_parser filename in 
  match file with 
  |(x,y) :: l'-> match (word_cutter y) with
  |"READ" ::l'-> (x,Read s)::(parse_instr lines (x+1));
  |"READ" ::[]-> failwith "vide";
  (*|"PRINT"::l'->;*)
  |"IF"::l'->;
  |"WHILE"::l'->;
  |"COMMENT" ::l'->;
  |[]-> failwith "vide"
  
;;
*)
let fetch_expr line :expr= 
  match line with
  |[]->failwith "vide"
;;
let fetch_comp str acc=
  match str with
  |"="->Eq
  |"<"->Lt
  |"<="->Le
  |">"->Gt
  |">="->Ge
  |"<>"->Ne
  |e->failwith "mauvais arg"
;;

let rec fetch_cond  acc line :cond= 
match line with
|"="::l'->(fetch_expr acc, Eq,fetch_expr l')
|"<"::l'->(fetch_expr acc, Lt,fetch_expr l')
|"<="::l'->(fetch_expr acc, Le,fetch_expr l')
|">"::l'->(fetch_expr acc, Gt,fetch_expr l')
|">="::l'->(fetch_expr acc, Ge,fetch_expr l')
|"<>"::l'->(fetch_expr acc,Ne ,fetch_expr l')
|s::l'->fetch_cond  (acc@[s]) l';

;;

let parse_if lines = 

let rec parse_instr lines (no:int) (no2:int)=
(*let curline =List.assoc no lines in ?*)
let line = List.assoc no lines in
let indent = indentation line in
let cutline = word_cutter line in
match  cutline with 
|"READ" :: s ::d -> (no2,Read s)::(parse_instr lines (no+1))
|"READ" ::[]-> failwith "vide"
|"PRINT" :: d' ->(no2, Print (fetch_expr d'))::(parse_instr lines (no+1) (no2+1))
|"IF" :: d' -> (no2,If((fetch_cond [] d'),parse_instr lines (no+1) (no2+1),parse_instr lines (no+1) (no2+1)))::parse_instr lines (no+1) (no2+1) (*CASSE*)
|"WHILE" :: d' -> parse_while d'
|"COMMENT" ::d'-> try
   let test = List.assoc (no+1) lines in 
   (parse_instr lines (no+1) no2) with Not_found ->[];
|s :: d :: l'->no2,Set (d(fetch_expr l'))
|s::d'->failwith "vide";
|[]->failwith "vide";
;;
(*les fonctions de read marchent jusqu'ici*)


(*






(*|"COMMENT"::d'-> Comment FETCH*)

let parse_if d =failwith "TODO";;

let parse_while d = failwith "TODO";;

let jump_comment d = failwith "TODO"

let read_line l no :int*instr =failwith "TODO"

let rec parse_instr lines (no:int) =
  (*let curline =List.assoc no lines in ?*)
  let line = List.assoc no lines in
  let indent = indentation line in
  let cutline = word_cutter line in
  match  cutline with 
  |"READ" :: s ::d -> (no,Read s)::(parse_instr lines (no+1))
  |"READ" ::[]-> failwith "vide"
  |"PRINT" :: d' ->(no, Print (fetch_expr d'))::(read_line lines (no+1))::[]
  |"IF"  :: d' -> parse_if d'
  |"WHILE" :: d' -> parse_while d'
  |"COMMENT" ::d'-> jump_comment d'
  |s::d'->failwith "vide";
  |[]->failwith "vide";
;;    
*)
*)

let read_polish (filename:string) : program = failwith "TODO"

let usage () =
  (*eval_polish abs;*) (*eval_polish factors;*) (*print_polish abs;*) (*print_polish factors;*)
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"
  
  let main () =
    match Sys.argv with
    | [|_;"-reprint";file|] -> Print.print_polish (Print.abs)
    | [|_;"-eval";file|] -> Eval.eval_polish (Eval.abs)
    | _ ->usage ()
    
    
    (* lancement de ce main *)
    let () = main ()
    
    
