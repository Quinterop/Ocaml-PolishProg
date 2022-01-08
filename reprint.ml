

let rec print_expr exp=
  match exp with
  |Type.Num(n) ->print_int n
  |Type.Var(s)->print_string s 
  |Type.Op(op,exp,exp2)->print_op op exp exp2 ;
  and 
  print_op op exp exp2 = 
  match op with
  |Type.Sub -> print_string "- "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Mul -> print_string "* "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Div -> print_string "/ "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Add -> print_string "+ "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Mod -> print_string "% "; print_expr exp; print_string " "; print_expr exp2
  ;;
  
  let print_comp comp expr1 expr2 = 
    match comp with 
    | Type.Eq -> print_expr expr1 ;print_string " = ";print_expr expr2
    | Type.Ne -> print_expr expr1 ;print_string " <> " ;print_expr expr2 
    | Type.Lt -> print_expr expr1 ;print_string " < ";print_expr expr2 
    | Type.Le -> print_expr expr1 ;print_string " <= " ;print_expr expr2 
    | Type.Gt -> print_expr expr1 ;print_string " > ";print_expr expr2 
    | Type.Ge -> print_expr expr1 ;print_string " >= " ;print_expr expr2 
  ;;
  let print_cond c :unit=
  let (exp,comp,exp2) = c in 
  print_comp comp exp exp2
  ;;

  (*FONCTIONS DE PRINT POLISH*)
  let rec print_expr exp=
  match exp with
  |Type.Num(n) ->print_int n
  |Type.Var(s)->print_string s 
  |Type.Op(op,exp,exp2)->print_op op exp exp2 ;
  and 
  print_op op exp exp2 = 
  match op with
  |Type.Sub -> print_string "- "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Mul -> print_string "* "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Div -> print_string "/ "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Add -> print_string "+ "; print_expr exp;  print_string " "; print_expr exp2
  |Type.Mod -> print_string "% "; print_expr exp; print_string " "; print_expr exp2
  ;;
  
  let print_comp comp expr1 expr2 = 
    match comp with 
    | Type.Eq -> print_expr expr1 ;print_string " = ";print_expr expr2
    | Type.Ne -> print_expr expr1 ;print_string " <> " ;print_expr expr2 
    | Type.Lt -> print_expr expr1 ;print_string " < ";print_expr expr2 
    | Type.Le -> print_expr expr1 ;print_string " <= " ;print_expr expr2 
    | Type.Gt -> print_expr expr1 ;print_string " > ";print_expr expr2 
    | Type.Ge -> print_expr expr1 ;print_string " >= " ;print_expr expr2 
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
  
  let print_polish (p:Type.program) : unit =
    let rec print_block p =
      match p with
      |[]->();
      |a::y -> match a with
      |_,Type.Set (n,e)->print_string (n ^" := "); (print_expr e) ;print_newline(); print_block y ;
      |_,Type.Read(n)->print_string("READ " ^ n);print_newline(); print_block y;
      |_,Type.If(c,bl,bl2)-> print_string "IF "; print_cond c ; print_newline(); print_block bl ;if not (check_empty_block bl2) then( print_string "ELSE"; print_newline(); print_block bl2); print_block y ;
      |_,Type.Print(e)-> print_string "PRINT "; print_expr e ;print_newline(); print_block y;
      |_,Type.While(c,b)->print_string "WHILE "; print_cond c; print_newline(); print_block b ; print_block y ;
    in print_block p
  ;;