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

open Ast
open Language

let print_ident fmt id = Format.fprintf fmt "%s" id
let print_break fmt () = Format.fprintf fmt "@ "
let print_comma fmt () = Format.fprintf fmt ",@ " (* pas top nom *)

let rec print_sep_list printer print_sep fmt = function
    [] -> ()
  | [e] -> printer fmt e
  | h::t -> printer fmt h;
            print_sep fmt ();
            print_sep_list printer print_sep fmt t

let rec print_term fmt = function
  | Const id -> Format.fprintf fmt "%s" id
  | Var id -> Format.fprintf fmt "%s" id
  | Func (id, tl) ->
     Format.fprintf fmt "(%s@ %a)"
                    id
                    (print_sep_list print_term print_break) tl

let print_atomic fmt = function
  | Equal (t1, t2) -> Format.fprintf fmt "%a@ =@ %a"
                                     print_term t1
                                     print_term t2
  | Rel (id, tl) -> Format.fprintf fmt "%s@ %a"
                                   id
                                   (print_sep_list print_term print_break) tl

let rec print_formula fmt = function
  | False -> Format.fprintf fmt "false"
  | Atomic a -> print_atomic fmt a
  | FVar v -> Format.fprintf fmt "%s" v
  | Not f -> Format.fprintf fmt "~%a" print_formula f
  | And (f, g) -> Format.fprintf fmt "(%a@ /\\@ %a)"
                                 print_formula f
                                 print_formula g
  | Or (f, g) -> Format.fprintf fmt "(%a@ \\/@ %a)"
                                print_formula f
                                print_formula g
  | Arrow (f, g) -> Format.fprintf fmt "(%a@ ->@ %a)"
                                   print_formula f
                                   print_formula g
  | Forall (v, f) ->
     Format.fprintf fmt "forall@ %s,@ %a"
                    v
                    print_formula f

  | Exists (v, f) ->
     Format.fprintf fmt "exists@ %s,@ %a"
                    v
                    print_formula f

let print_arg fmt = function
  | Ident id -> Format.fprintf fmt "%s" id
  | UppIdent id -> Format.fprintf fmt "%s" id
  | Term t -> Format.fprintf fmt "%a" print_term t
  | Formula f -> Format.fprintf fmt "%a" print_formula f

let print_s_type fmt = function
  | Prefix n -> Format.fprintf fmt "%i" n
  | Infix -> Format.fprintf fmt "infix"

let print_decl fmt (id, t) =
  Format.fprintf fmt "%s : %a"
                 id
                 print_s_type t

let print_command fmt = function
  | Language (consts, funcs, rels) ->
     Format.fprintf fmt "@[Language : { %a }@ { %a }@ { %a }.@]@."
                    (print_sep_list print_ident print_comma) consts
                    (print_sep_list print_decl print_comma) funcs
                    (print_sep_list print_decl print_comma) rels
  | Axiom (id, f) ->
     Format.fprintf fmt "@[Axiom %s := %a.@]@."
                    id
                    print_formula f
  | Theorem (id, f) ->
     Format.fprintf fmt "@[Theorem %s := %a.@]@."
                    id
                    print_formula f
  | Proof -> Format.fprintf fmt "Proof.@."
  | Qed -> Format.fprintf fmt "Qed.@."
  | Tactic (id, args) ->
     Format.fprintf fmt "@[  %s@ %a.@]@."
                    id
                    (print_sep_list print_arg print_break) args

let print_hyp fmt (id, f) =
  Format.fprintf fmt "@[%s : %a@]"
                 id
                 print_formula f

let print_goal fmt (hyp, g) =
  Format.fprintf fmt "@[<v>%a@ @ goal : @[%a@]@]"
                 (print_sep_list print_hyp print_break) hyp
                 print_formula g

let print_program fmt p =
  Format.fprintf fmt "@[<v>%a@]@."
                 (print_sep_list print_command print_break) p
