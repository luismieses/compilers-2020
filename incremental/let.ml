open Printf
open Int64
   
type expr =
  | Num of int64
  | Add1 of expr
  | Sub1 of expr
  | Id of string
  | Let of string * expr * expr

type reg = 
  | RSP                         (* Stack pointer *)
  | RAX

type arg = 
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg

type env = (string * int) list

let reg_to_string reg =
  match reg with
  | RAX -> "RAX"
  | RSP -> "RSP"

let arg_to_string arg =
  match arg with
  | Const n -> Int64.to_string n
  | Reg reg -> reg_to_string reg
  | RegOffset (reg, off) -> "[" ^ (reg_to_string reg) ^ " + 8*" ^ (string_of_int off) ^ "]"

let rec asm_to_string (asm : instruction list) : string =
  (* les toca a ustedes *)
  match asm with
  | [] -> ""
  | IMov (arg1, arg2)::tail -> "mov " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IAdd (arg1, arg2)::tail -> "add "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
;;

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

(* compile is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let rec compile (e : expr) (env : env) : instruction list =
  match e with
  | Num n -> [ IMov(Reg(RAX), Const(n)) ]
  | Add1 e -> (compile e env) @ [ IAdd(Reg(RAX), Const (1L)) ]  
  | Sub1 e -> (compile e env) @ [ IAdd(Reg(RAX), Const (-1L)) ]
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
  ;;

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

(* Some OCaml boilerplate for reading files and command-line arguments *)

(* need a real parser now!
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let input_program = Int64.of_string (input_line input_file) in
  let program = (compile_prog input_program) in
  printf "%s\n" program;;
*)
