
let rec print_expr exp =
  match exp with
  |Type.Num(n) ->print_int n
  |Type.Var(s)->print_string s 
  |Type.Op(op,exp,exp2)->print_op op exp exp2 ;
  and 
  print_op op exp exp2 = 
  match op with
  |Type.Sub -> print_string "- "; print_expr exp;  print_string " "; print_expr exp2;
  |Type.Mul -> print_string "* "; print_expr exp;  print_string " "; print_expr exp2;
  |Type.Div -> print_string "/ "; print_expr exp;  print_string " "; print_expr exp2;
  |Type.Add -> print_string "+ "; print_expr exp;  print_string " "; print_expr exp2;
  |Type.Mod -> print_string "% "; print_expr exp; print_string " "; print_expr exp2;
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


let rec simpl_exp exp =
  match exp with
  | Type.Var(v)-> Type.Var(v)
  | Type.Num(n) -> Type.Num(n)
  
  |Type.Op(op, exp1, exp2) ->
    begin
      let pexp1 = simpl_exp exp1 in
      let pexp2 = simpl_exp exp2 in 
      
      match pexp1 with 
      |Type.Var(n) ->
        begin
          match pexp2 with
          
          |Type.Var(n) -> Type.Op(op, pexp1, pexp2) (*cas var,var*)
          
          |Type.Num(x) ->        (*cas var,num*)
            begin
              match op with 
              |Type.Add -> if x = 0 then Type.Var(n) else Type.Op(op, pexp1, Type.Num(x))
              |Type.Sub -> if x = 0 then Type.Var(n) else Type.Op(op, pexp1, Type.Num(x))
              |Type.Mul -> if x = 0 then Type.Num(0) else Type.Op(op, pexp1, Type.Num(x))
              |others -> Type.Op(op, pexp1, Type.Num(x))
            end
          end
          
          |Type.Num(x) -> 
            begin 
              match pexp2 with
              |Type.Var(n) ->  (*cas num, var*)
                begin 
                  match op with 
                  |Type.Add -> if x = 0 then Type.Var(n) else Type.Op(op, Type.Num(x), pexp2)
                  |Type.Mul -> if x = 0 then Type.Num(0) else Type.Op(op, Type.Num(x), pexp2)
                  |others -> Type.Op(op, Type.Num(x), pexp2)
                end
                |Type.Num(y) -> let res = Eval.eval_op op (Type.Num(x)) (Type.Num(y)) in Type.Num(res) (*cas num, num*)
              end
            end
          ;;

let simpl_cond con :Type.cond= 
let exp1,comp,exp2 = con in
let simpl_exp1 = simpl_exp exp1 in
let simpl_exp2 = simpl_exp exp2 in
match simpl_exp1 with
|Type.Num(x)->  begin 
  let val_exp1 = x in
  match simpl_exp2 with 
  |Type.Num(x) -> if Eval.eval_comp comp simpl_exp1 simpl_exp2 then (Num(1),Type.Eq,Num(1)) else (Num(1),Type.Eq,Num(0));
  |other -> (Type.Num(val_exp1),comp,simpl_exp2)
end
|other -> match simpl_exp2 with 
|Type.Num(x) -> (simpl_exp1,comp,Type.Num(x))
|other -> (simpl_exp1,comp,simpl_exp2)
;;

let rec simpli_polish block accu = 
  match block with
  |[] -> accu
  |(x,instr)::rest-> 
    match instr with
    |Type.Set(n,e) -> simpli_polish rest (List.append accu [(x,(Type.Set(n,(simpl_exp e))))])
    |Type.Read(n) -> simpli_polish rest (List.append accu [(x,Type.Read(n))])
    |Type.Print(e) -> simpli_polish rest (List.append accu [(x,Type.Print(simpl_exp e))])
    
    |Type.While(cond, b)-> 
      begin
        let simp_cond= simpl_cond cond in
        let bs=simpli_polish b [] in
        match simp_cond with 
        |(Num(1),Type.Eq,Num(1))-> simpli_polish rest (List.append accu [(x,Type.While((Type.Num(1),Type.Eq,Type.Num(1)),bs))])
        |(Num(1),Type.Eq,Num(0))-> simpli_polish rest accu
        |other -> simpli_polish rest (List.append accu [(x,Type.While(simp_cond,bs))])
      end 
      
      |Type.If(cond,b1,b2) -> 
        let bs1=simpli_polish b1 [] in
        let bs2=simpli_polish b2 [] in 
        let simp_cond= simpl_cond cond in
        match simp_cond with  
        |(Num(1),Type.Eq,Num(1))-> simpli_polish rest (List.append accu [(x,Type.If(simp_cond,bs1,[]))])
        |(Num(1),Type.Eq,Num(0)) -> simpli_polish rest (List.append accu [(x,Type.If(simp_cond,bs2,[]))])
        |other -> simpli_polish rest (List.append accu [(x,Type.If(simp_cond,bs1,bs2))])
      ;;
      
let simpl_polish bl = simpli_polish bl []