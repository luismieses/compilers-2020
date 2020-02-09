let print_binop op =
  match op with
  | Add -> print_string " + "
  | Sub -> print_string " - "
  | Mult -> print_string " * "
  | Div -> print_string " / " 

let rec print_exp expr =
  match expr with
  | Num n -> print_int n
  | BinOp (e1, op, e2) -> print_exp e1; print_binop op ; print_exp e2
  | Name id -> print_string id

let rec print_list_exp lst =
  match lst with
  | [] -> print_newline ()
  | exp :: exprs -> print_exp exp ; print_string " " ; print_list_exp exprs

let print_stm stm =
  match stm with
  | Expr exp -> print_exp exp ; print_newline ()
  | Print lst -> print_list_exp lst
  | Assign (id, exp) -> print_string id ; 
                        print_string " = "; 
                        print_exp exp ;
                        print_newline ()

let print_prog = function
  | Module stms -> List.map print_stm stms ; ()
