(* Escribir AST aqui  *)
type binop = Plus | Minus | Mult | And | Or | Less
            | LessEq | Greater | GreaterEq | Eq | Ne

type expr =
  | Num of int64
  | Add1 of expr
  | Sub1 of expr
  | BinOp of expr * binop * expr
  | Id of string
  | Bool of bool
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Not of expr

type decl =
  | Func of string * string list * expr

type program =
  | Decls of decl list * expr
  | Expr of expr
