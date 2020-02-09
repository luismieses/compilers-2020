type id = string

type binop = Add | Sub | Mult | Div

type exp =
    | Num of int
    | BinOp of exp * binop * exp
    | Name of id

type stm =
    | Assign of id * exp
    | Print of exp list
    | Expr of exp

type prog =
    | Module of stm list

let p1 = Module [Expr (Num 123)];;

let p2 = Module [Expr (BinOp (Num 3, Add, Num 2))];;

let p3 = Module [Assign ("a", Num 3); Print [BinOp (Name "a", Mult, Num 2)]];;
