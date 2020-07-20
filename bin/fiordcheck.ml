(******************************************************************************)
(*  Copyright 2019-2020 Benjamin Terra-Jorge                                  *)
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

open Fiord
open Interpreter

let get_lang = function
  | Init -> Language.empty
  | Running (l, _) -> l

(* Print the name of proven theorems *)
let folder ctx c =
  match (ctx, c, interpret ctx c) with
  | (Running (_, InProof (_, (id, _), _)), Qed,
     (Running (_, Main _) as ctx)) ->
     print_endline ("theorem " ^ id ^ " proven.");
     ctx
  | _ -> interpret ctx c

let () =
  let ic = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel ic in

  let c = Parser.sentence (Lexer.token Language.empty) lexbuf in

  let ctx = interpret Init c in

  let p = Parser.program (Lexer.token (get_lang ctx)) lexbuf in

  let _ = List.fold_left folder ctx p in
  ()
