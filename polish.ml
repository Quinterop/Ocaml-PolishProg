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


(***********************************************************************)
  (** découpe les fichiers en lignes et les met dans une int * string list *)
let rec add_lines input (no:int) lines  =
try
  add_lines(input)(no+1) (List.cons (no,(input_line input))lines)
with End_of_file ->close_in input;lines;;

let line_parser filename =
  try
  let ic = open_in filename in 
  let (lines: (int * string) list) = [] in
  List.rev (add_lines ic 1 lines )
  with e ->failwith "fichier non ouvrable"
;;
  
  

  let word_cutter line =  failwith "TODO"
  
(*prend un string et renvoie un int correspondant aux espaces avant le premier autre caractere*)
  let tabify s = List.init (String.length s) (String.get s);;
  let rec indentation s= 
  let t = tabify s in
  let rec indent_aux t n =
    match t with
    | [] -> n
    |c::t'-> if c =' ' then indent_aux (t') (n+1) else n in 
    indent_aux t 0
  ;;
  

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
      