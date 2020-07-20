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

{
  open Parser
  open Fiord.Language

  let choose default lang (id : string) =
    match Fiord.Language.get_type lang id with
    | None -> default id
    | Some st ->
       match st with
       | Constant -> CONST id
       | Function (Prefix _) -> FUNC id
       | Function Infix -> BINOP id
       | Relation (Prefix _) -> REL id
       | Relation Infix -> BINREL id
}

let lower = [ 'a' - 'z' ]
let upper = [ 'A' - 'Z' ]
let digit = [ '0' - '9' ]
let other = [ '+' '-' '*' '/' '^' '@' '$' '|' '&' '!' '?' '~' '<' '>' '=']

let ident = lower (lower | upper | digit | '_') *
let upper_ident = upper (lower | upper | digit | '_') * (* nom ? *)
let number = digit +
let symbol = other +

rule token lang = parse
| [ ' ' '\t' '\n' '\r' ] { token lang lexbuf }
| "(*"                   { comment lang 1 lexbuf }
| '='                    { EQUAL }
| '~'                    { NOT }
| "/\\"                  { AND }
| "\\/"                  { OR }
| "->"                   { ARROW } (* IMPLIES ? *)
| "<->"                  { EQUIV }
| "exists"               { EXISTS }
| "forall"               { FORALL }
| "false"                { FALSE }
| ','                    { COMMA }
| '('                    { LPAREN }
| ')'                    { RPAREN }
| '{'                    { LBRACE }
| '}'                    { RBRACE }
| ':'                    { COLON }
| '.'                    { DOT }
| ":="                   { DEFINED }
| "infix"                { INFIX }
| "Language"             { LANGUAGE }
| "Definition"           { DEFINITION }
| "Axiom"                { AXIOM }
| "Theorem"              { THEOREM }
| "Proof"                { PROOF }
| "Qed"                  { QED }
| ident as id            { choose (fun x -> IDENT x) lang id }
| upper_ident as id      { choose (fun x -> UPPIDENT x) lang id }
| number as num          { choose (fun x -> NUMBER x) lang num }
| number as num "%int"   { NUMBER num }
| symbol as symb         { choose (fun x -> SYMBOL x) lang symb }
| eof                    { EOF }
| _ as c                 { print_string "unexpected char: ";
                           print_char c;
                           print_newline ();
                           exit 1 }


and comment lang n = parse
| "(*"  { comment lang (n + 1) lexbuf }
| "*)"  { if n > 1 then comment lang (n - 1) lexbuf else token lang lexbuf }
| _     { comment lang n lexbuf }
