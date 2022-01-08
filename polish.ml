

(***********************************************************************)
(* FONCTIONS DE READ POLISH*)
(*découpe les fichiers en lignes et les met dans une int * string list. *)
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
let read_polish (filename:string) : Type.program = 
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
let fetch_op line acc :Type.op= 
match line with
|"+"::l'->(fetch_expr acc, Type.Add,fetch_expr l')
|"-"::l'->(fetch_expr acc, Type.Sub,fetch_expr l')
|"*"::l'->(fetch_expr acc, Type.Mul,fetch_expr l')
|"/"::l'->(fetch_expr acc, Type.Div,fetch_expr l')
|"%"::l'->(fetch_expr acc, Type.Mod,fetch_expr l')
|s::l'->fetch_cond  (acc@[s]) l';
|[]-> failwith "vide";
;;

let fetch_num = ;;

let fetch_int =;;

let fetch_expr line =
match line with
| ;;

let fetch_comp str acc=
match str with
|"="->Type.Eq
|"<"->Type.Lt
|"<="->Type.Le
|">"->Type.Gt
|">="->Type.Ge
|"<>"->Type.Ne
|e->failwith "mauvais arg"
;;

let rec fetch_cond  acc line :Type.cond= 
match line with
|"="::l'->(fetch_expr acc, Type.Eq,fetch_expr l')
|"<"::l'->(fetch_expr acc, Type.Lt,fetch_expr l')
|"<="::l'->(fetch_expr acc, Type.Le,fetch_expr l')
|">"::l'->(fetch_expr acc, Type.Gt,fetch_expr l')
|">="::l'->(fetch_expr acc, Type.Ge,fetch_expr l')
|"<>"::l'->(fetch_expr acc,Type.Ne ,fetch_expr l')
|s::l'->fetch_cond  (acc@[s]) l';
|[]-> failwith "vide";
;;

let parse_if lines =()



let rec parse_block lines no ind :Type.program=

let rec parse_instr (no:int) (no2:int) :Type.block=

let line = try List.assoc no lines with Not_found -> "finduficher"; in
if line <> "findufichier" then (
let indent = indentation line in
if indent >= ind then (
let cutline = word_cutter line in
match cutline with 
|"READ" :: s ::d' -> (no2,Read s)::(parse_instr (no+1)(no2+1))
|"READ" ::[]-> failwith "vide"

|"PRINT" :: d' ->(no2, Print (fetch_expr d'))::(parse_instr (no+1) (no2+1))

|"IF" :: d' ->
let cond = fetch_cond [] d' in
let block1 =  parse_block lines (no+1) (ind+2) in 
let block2 =  parse_block lines (no+1) (ind+2) in 
(no2,(If (cond, block1, block2))) :: parse_instr (no+1) (no2+1);

|"WHILE" :: d' ->let condition = fetch_cond [] d' in 
  let block = parse_block lines no (ind+2) in
  (no2,While (condition, block)) :: parse_instr (no+1) (no2+1);
|[]->failwith "vide"; 

|"COMMENT" ::d'-> let check_comment no1 no2 line = try (let test = List.assoc (no+1) lines in (parse_instr (no+1) no2)) with Not_found
 -> [] in check_comment no no2 lines;

|s :: d' -> (no2,Set (s,fetch_expr d'))::(parse_instr (no+1)(no2+1));
)
else [];
) else [];
in parse_instr no 0;

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


let read_polish (filename:string) : Type.program = 
    parse_block (line_parser filename) 0 0 ;;

let usage () =
  (*eval_polish abs;*) (*eval_polish factors;*) (*print_polish abs;*) (*print_polish factors;*)
 
  Reprint.print_polish(read_polish "exemples/abs.p");;
  
  let main () =
    match Sys.argv with
    (*| [|_;"-reprint";file|] -> Reprint.print_polish (Type.abs)
    | [|_;"-eval";file|] -> Eval.eval_polish (Type.abs)*)
    | _ ->usage ()
  
    ;;
    (* lancement de ce main *)
    let () = main ();;
    
    
    