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

{
  open Parser

  exception End_of_input

  let choose (gctx, (cl, fl, rl)) id =
    if List.mem id cl then
      CONST id
    else if List.mem id (List.map fst fl) then
      FUNC id
    else if List.mem id (List.map fst rl) then
      REL id
    else if List.mem id (List.map fst gctx) then
      FVAR id  (* pas vraiment plaisant de lexer à partir du contexte *)
    else       (* (si y avait que le language, j'dit pas, mais les formules! *)
      IDENT id
}

let lower = [ 'a' - 'z' ]
let upper = [ 'A' - 'Z' ]
let digit = [ '0' - '9' ]

let ident = (lower | upper) (lower | upper | digit | '_') *
let number = digit +

rule token lang = parse
| [ ' ' '\t' '\n' '\r' ] { token lang lexbuf }
| "(*"                   { comment lang 1 lexbuf }
| '='                    { EQUAL }
| '~'                    { NOT }
| "/\\"                  { AND }
| "\\/"                  { OR }
| "->"                   { ARROW } (* IMPLIES ? *)
| "exists"               { EXISTS }
| "forall"               { FORALL }
| "false"                { FALSE }
| ','                    { COMMA }
| '('                    { LPAREN }
| ')'                    { RPAREN }
| '{'                    { RBRACE }
| '}'                    { LBRACE }
| ':'                    { COLON }
| '.'                    { DOT }
| ":="                   { DEFINED }
| "Language"             { LANGUAGE }
| "Axiom"                { AXIOM }
| "Theorem"              { THEOREM }
| "Proof"                { PROOF }
| "Qed"                  { QED }
| ident as id            { choose lang id }
| number as num          { NUMBER (int_of_string num) }
| eof                    { raise End_of_input }
| _ as c                 { print_string "unexpected char: ";
                           print_char c;
                           print_newline ();
                           exit 1 }

and comment lang n = parse
| "(*"  { comment lang (n + 1) lexbuf }
| "*)"  { if n > 1 then comment lang (n - 1) lexbuf else token lang lexbuf }
| _     { comment lang n lexbuf }
