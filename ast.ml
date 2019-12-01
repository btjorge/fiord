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

type ident = string

type decl = ident * int

type term = Const of ident
          | Var of ident
          | Func of ident * term list

type atomic = Equal of term * term
            | Rel of ident * term list

type formula = False
             | Atomic of atomic
             | FVar of ident
             | Not of formula
             | And of formula * formula
             | Or of formula * formula
             | Arrow of formula * formula
             | Forall of ident list * formula
             | Exists of ident list * formula

type argument = Ident of ident
              | Term of term
              | Formula of formula

type tactic = ident * argument list

type command = Language of ident list * decl list * decl list
             | Axiom of ident * formula
             | Theorem of ident * formula
             | Proof
             | Qed
             | Tactic of tactic

type program = command list
