open Printf
open Int64

type expr =
  | Num of int64
  | Add1 of expr
  | Sub1 of expr

type reg = 
  | EAX
  | RAX

type arg = 
  | Const of int64
  | Reg of reg

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg

let arg_to_string arg =
  match arg with
  | Const n -> Int64.to_string n
  | Reg RAX -> "RAX"
  | Reg EAX -> "EAX"

let rec asm_to_string (asm : instruction list) : string =
  (* les toca a ustedes *)
  match asm with
  | [] -> ""
  | IMov (arg1, arg2)::tail -> "mov " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IAdd (arg1, arg2)::tail -> "add "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
;;

(* REFACTORING STARTS HERE *)
(* compile_expr is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let rec compile_expr (e : expr) : instruction list =
  match e with
  | Num n -> [ IMov(Reg(RAX), Const(n)) ]
  | Add1 e -> (compile_expr e) @ [ IAdd(Reg(RAX), Const (1L)) ]  
  | Sub1 e -> (compile_expr e) @ [ IAdd(Reg(RAX), Const (-1L)) ]  
  ;;

(* compile_prog surrounds a compiled program by whatever scaffolding is needed *)
let compile_prog (e : expr) : string =
  (* compile the program *)
  let instrs = compile_expr e in
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
