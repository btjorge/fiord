(******************************************************************************)
(*  Copyright 2019 Benjamin Terra-Jorge                                       *)
(*                                                                            *)
(*  Licensed under the Apache License, Version 2.0 (the "License");           *)
(*  you may not use this file except in compliance with the License.          *)
(*  You may obtain a copy of the License at                                   *)
(*                                                                            *)
(*      http://www.apache.org/licenses/LICENSE-2.0                            *)
(*                                                                            *)
(*  Unless required by applicable law or agreed to in writing, software       *)
(*  distributed under the License is distributed on an "AS IS" BASIS,         *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  *)
(*  See the License for the specific language governing permissions and       *)
(*  limitations under the License.                                            *)
(*                                                                            *)
(******************************************************************************)

open Tactics
open Interpreter
open Print

let read_sentence () =
  let rec loop acc =
    let l = read_line () in
    let lenl = String.length l in

    if lenl > 0 && String.get l (lenl - 1) = '.' then
      acc ^ "\n" ^ l (* we need some blank to separate tokens *)
    else
      begin
        print_string "     > "; (* second level prompt. pas top *)
        loop (acc ^ l)
      end
  in

  print_string "fiord> "; (* print a prompt. pas top *)

  loop ""

let get_language  = function
    Init -> Language.empty
  | Running (l, _) ->  l

let rec loop ctx =

  let s = read_sentence () in
  let lexbuf = Lexing.from_string s in

  begin
    try
      let command = Parser.sentence (Lexer.token (get_language ctx)) lexbuf in

      try
        let ctx = interpret ctx command in

        (match ctx with
         | Running (_, InProof (_, _, goal :: _)) ->
            print_goal Format.std_formatter goal;
            Format.print_newline ()
         | Running (_, InProof (_, _, [])) ->
            print_endline "No more goal to prove"
         | _ -> ());
        loop ctx
      with
        TacticError s -> print_endline ("TacticError: " ^ s)
      | ContextError -> print_endline "ContextError"
    with
      Parser.Error -> print_endline "SyntaxError"
  end;

  loop ctx

let () =
  try loop Init with End_of_file -> print_newline ()
