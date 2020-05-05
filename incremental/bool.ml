open Printf
open Int64

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

type reg = 
  | RSP                         (* Stack pointer *)
  | RAX
  | RBX

type arg = 
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

type instruction = 
  | IJl of string
  | IJg of string
  | IJle of string
  | IJge of string
  | IJne of string
  | IMov of arg * arg
  | IAdd of arg * arg
  | ICmp of arg * arg
  | IJe of string
  | ILabel of string
  | IJump of string
  | ISub of arg * arg
  | IMul of arg 
  | ISar of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg



type env = (string * int) list

let reg_to_string reg =
  match reg with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | RBX -> "RBX"

let arg_to_string arg =
  match arg with
  | Const n -> Int64.to_string n
  | Reg reg -> reg_to_string reg
  | RegOffset (reg, off) -> "[" ^ (reg_to_string reg) ^ " + 8*" ^ (string_of_int off) ^ "]"

let rec asm_to_string (asm : instruction list) : string =
  (* les toca a ustedes *)
  match asm with
  | [] -> ""
  | ISub (arg1, arg2)::tail -> "sub " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IMov (arg1, arg2)::tail -> "mov " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IAdd (arg1, arg2)::tail -> "add "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | ICmp (arg1, arg2)::tail -> "cmp " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IJe label::tail -> "je " ^ label ^ "\n" ^ asm_to_string tail
  | ILabel label::tail -> label ^ ":\n" ^ asm_to_string tail
  | IJump label::tail -> "jmp " ^ label ^ "\n" ^ asm_to_string tail
  | IMul (arg1)::tail -> "mul " ^  arg_to_string arg1 ^ "\n" ^ asm_to_string tail
  | ISar (arg1, arg2)::tail -> "sar " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IAnd (arg1, arg2)::tail -> "and "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IOr (arg1, arg2)::tail -> "or "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IXor (arg1, arg2)::tail -> "xor "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IJle label::tail -> "jle " ^ label ^ "\n" ^ asm_to_string tail
  | IJg label::tail -> "jg " ^ label ^ "\n" ^ asm_to_string tail
  | IJl label::tail -> "jl " ^ label ^ "\n" ^ asm_to_string tail
  | IJge label::tail -> "jge " ^ label ^ "\n" ^ asm_to_string tail
  | IJne label::tail -> "jne " ^ label ^ "\n" ^ asm_to_string tail








let rec lookup name env =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in environment" name)
  | (n, i)::rest ->
     if name = n then i else (lookup name rest)
;;

let add name env =
  let slot = 1 + (List.length env) in
  ((name,slot)::env, slot)
;;

let gensym =
  let counter = ref 0 in
  (fun basename ->
    counter := !counter + 1;
    sprintf "%s_%d" basename !counter);;

let is_imm e =
  match e with
  | Num _ -> true
  | Id _ -> true
  | _ -> false

let rec is_anf e =
  match e with
  | Add1 e -> is_imm e
  | Sub1 e -> is_imm e
  | BinOp (e1, _, e2) -> is_imm e1 && is_imm e2
  | Let (_, e1, e2) -> is_anf e1 && is_anf e2
  | If (cond, thn, els) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e 

let rec anf_v1 e =
  match e with
  | Add1 e1 ->
    let (e1_ans, e1_context) = anf_v1 e1 in
    let temp = gensym "add1" in
    (Id(temp), (* the answer *)
     e1_context @ (* the context needed for the left answer to make sense *)
     [(temp, Add1(e1_ans))]) (* definition of the answer *)
  | Sub1 e1 ->
    let (e1_ans, e1_context) = anf_v1 e1 in
    let temp = gensym "sub1" in
    (Id(temp), (* the answer *)
     e1_context @ (* the context needed for the left answer to make sense *)
     [(temp, Sub1(e1_ans))]) (* definition of the answer *)

  | BinOp (e1, op, e2) ->
     let (left_ans, left_context) = anf_v1 e1 in
     let (right_ans, right_context) = anf_v1 e2 in
     let temp = gensym "binop" in
       (Id(temp), left_context @ right_context @ [(temp, BinOp (left_ans, op, right_ans))])
  | Num _ -> (e, [])
  | If (cond, thn, els) ->
    let(cond_ans,cond_context) = anf_v1 cond in 
    let (thn_ans, thn_context) = anf_v1 thn in
    let (els_ans, els_context) = anf_v1 els in
    let cond_temp = gensym "cond" in
     (Let (cond_temp,cond_ans, If(Id cond_temp,thn_ans,els_ans)),
      thn_context @ els_context @cond_context)
  | Let (id,e2,e3) ->

    let (id_ans, id_context) = anf_v1 e2 in
    let (env_ans, env_context) = anf_v1 e3 in
    let temp = gensym "let" in
    (Id(temp), id_context @ env_context  @ [(temp, Let (id, id_ans, env_ans))])

let rec anf_helper e context =
  match context with
  | [] -> e
  | (id, e2) :: tail -> Let (id, e2, anf_helper e tail)

let anf (e : expr) : expr =
  let (e, context ) = anf_v1 e in
    anf_helper e context


let min_cobra_int = Int64.div Int64.min_int 2L
let max_cobra_int = Int64.div Int64.max_int 2L
let const_true  =   0xFFFFFFFFFFFFFFFFL
let const_false =   0x7FFFFFFFFFFFFFFFL
let const_bool_mask=0x8000000000000000L
(* compile is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let rec compile (e : expr) (env : env) : instruction list =
  match e with
  | Num n -> if n > max_cobra_int || n < min_cobra_int then failwith ("Integer overflow " ^ (Int64.to_string n))
   else
      [ IMov(Reg(RAX), Const(Int64.shift_left n 1)) ]
  | Bool true -> (*valor cierto*) [ IMov(Reg(RAX), Const(const_true)) ]


  | Bool false -> (*valor falso*) [ IMov(Reg(RAX), Const(const_false)) ]


  | Add1 e -> (compile e env) @ [ IAdd(Reg(RAX), Const (Int64.shift_left 1L 1)) ]  
  | Sub1 e -> (compile e env) @ [ IAdd(Reg(RAX), Const (-2L)) ]
  | BinOp (e1, Plus, e2) ->
     (compile e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile e2 env) @
     [ IAdd (Reg(RAX), Reg(RBX))]   (* sumar - cuidado con orden argumentos *)
  | BinOp (e1, Minus, e2) ->
     (compile e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile e2 env) @
     [ ISub (Reg(RAX), Reg(RBX))]   (* restar - cuidado con orden argumentos *)  
  | BinOp (e1, Mult, e2) ->
     (compile e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile e2 env) @
     [ IMul (Reg(RBX))]   (* muliplicar - cuidado con orden argumentos *)
     @[ISar (Reg(RAX), Const(1L))]
  | BinOp (e1, And, e2) ->
     (compile e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile e2 env) @
     [ IAnd (Reg(RAX), Reg(RBX))]   (* sumar - cuidado con orden argumentos *)
  | BinOp (e1, Or, e2) ->
     (compile e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile e2 env) @
     [ IOr (Reg(RAX), Reg(RBX))]   (* sumar - cuidado con orden argumentos *)

  | Not e -> [IMov(Reg(RBX), Const(const_bool_mask))] @ (compile e env) @ [ IXor(Reg(RAX), Reg(RBX)) ]





  | Id name -> let slot = (lookup name env) in
               [ IMov(Reg(RAX), RegOffset(RSP, ~-1 * slot) ) ]
  | Let (x, e, b) ->
     let (env', slot) = add x env in
     (* Compile the binding, and get the result into RAX *)
     (compile e env)
     (* Copy the result in RAX into the appropriate stack slot *)
     @ [ IMov(RegOffset(RSP, ~-1 * slot), Reg(RAX)) ]
     (* Compile the body, given that x is in the correct slot when it's needed *)
     @ (compile b env')
  | If (e1, e2, e3) ->
     let else_label = gensym "else_branch" in
     let done_label = gensym "done" in
     compile e1 env
     @ [ ICmp (Reg(RAX), Const(0L)) ;
         IJe (else_label) ] @
       compile e2 env
       @ [ IJump done_label ]
       @ [ ILabel else_label ]
       @ compile e3 env
       @ [ ILabel done_label ]


  | BinOp (e1, Less, e2) ->
     let less_label = gensym "less" in
     let great_label = gensym "greater" in
     let done_label = gensym "done" in
     compile e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJl (less_label) ] @
       [ILabel great_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel less_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, LessEq, e2) ->
     let lessEq_label = gensym "lessEq" in
     let great_label = gensym "greater" in
     let done_label = gensym "done" in
     compile e2 env   
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJle (lessEq_label) ] @
       [ILabel great_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel lessEq_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, Greater, e2) ->
     let less_label = gensym "less" in
     let greater_label = gensym "greater" in
     let done_label = gensym "done" in
     compile e2 env   
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJg (greater_label) ] @
       [ILabel less_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel greater_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, GreaterEq, e2) ->
     let less_label = gensym "less" in
     let greaterEq_label = gensym "greaterEq" in
     let done_label = gensym "done" in
     compile e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJge (greaterEq_label) ] @
       [ILabel less_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel greaterEq_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, Eq, e2) ->
     let eq_label = gensym "Equal" in
     let ne_label = gensym "NotEqual" in
     let done_label = gensym "done" in
     compile e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJe (eq_label) ] @
       [ILabel ne_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel eq_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, Ne, e2) ->
     let eq_label = gensym "Equal" in
     let ne_label = gensym "NotEqual" in
     let done_label = gensym "done" in
     compile e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJne (ne_label) ] @
       [ILabel eq_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel ne_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]






(* compile_prog surrounds a compiled program by whatever scaffolding is needed *)
let compile_prog (e : expr) : string =
  (* compile the program *)
  let instrs = compile e [] in
  (* convert it to a textual form *)
  let asm_string = asm_to_string instrs in
  (* surround it with the necessary scaffolding *)
  let prelude = "
section .text
global our_code_starts_here
our_code_starts_here:" in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_string ^ "\n" ^ suffix
  ;;

