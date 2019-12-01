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

open Ast

let rec rename_term x y = function
  | Const c -> Const c
  | Var z -> if x = z then Var y else Var z
  | Func (id, tl) -> Func (id, List.map (rename_term x y) tl)

let rename_atomic x y = function
  | Equal (t1, t2) -> Equal (rename_term x y t1, rename_term x y t2)
  | Rel (id, tl) -> Rel (id, List.map (rename_term x y) tl)

let rec rename x y = function
  | False -> False
  | Atomic a -> Atomic (rename_atomic x y a)
  | FVar v -> FVar v
  | Not f -> Not (rename x y f)
  | And (f, g) -> And (rename x y f, rename x y g)
  | Or (f, g) -> Or (rename x y f, rename x y g)
  | Arrow (f, g) -> Arrow (rename x y f, rename x y g)
  | Forall (vars, f) -> if List.mem x vars then
                          Forall (vars, f)
                        else
                          Forall (vars, rename x y f)
  | Exists (vars, f) -> if List.mem x vars then
                          Exists (vars, f)
                        else
                          Exists (vars, rename x y f)

(* Corriger ces fonctions *)
let rec substitute_term x t = function
  | Const c -> Const c
  | Var y -> if x = y then t else Var y
  | Func (id, tl) -> Func (id, List.map (substitute_term x t) tl)

let substitute_atomic x t = function
  | Equal (t1, t2) -> Equal (substitute_term x t t1, substitute_term x t t2)
  | Rel (id, tl) -> Rel (id, List.map (substitute_term x t) tl)

let rec substitute x t = function
  | False -> False
  | Atomic a -> Atomic (substitute_atomic x t a)
  | FVar v -> FVar v
  | Not f -> Not (substitute x t f)
  | And (f, g) -> And (substitute x t f, substitute x t g)
  | Or (f, g) -> Or (substitute x t f, substitute x t g)
  | Arrow (f, g) -> Arrow (substitute x t f, substitute x t g)
  | Forall (vars, f) -> if List.mem x vars then
                          Forall (vars, f)
                        else
                          Forall (vars, substitute x t f)
  | Exists (vars, f) -> if List.mem x vars then
                          Exists (vars, f)
                        else
                          Exists (vars, substitute x t f)
