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
open Formula

exception TacticError of string

let tactic_error s = raise (TacticError s)

let as_name = function
  | Ident id -> id
  | UppIdent id -> id
  | Term _ -> tactic_error "not a name"
  | Formula _ -> tactic_error "not a name"

let as_term = function
  | Ident id -> Var id
  | UppIdent _ -> tactic_error "not a term"
  | Term t -> t
  | Formula _ -> tactic_error "not a term"

let as_formula = function
  | Ident _ -> tactic_error "not a formula"
  | UppIdent id -> FVar (id, []) (* sûr ? *)
  | Term _ -> tactic_error "not a formula"
  | Formula f -> f

(* Inspecter correctement cette fonction. *)
let interpret_tactic glob (hyps, f) = function
  | ("intro_arrow", [UppIdent h]) ->
     begin
       match f with
       | Arrow (f, g) -> [((h, f) :: hyps, g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_arrow", [g]) ->
     [(hyps, Arrow (as_formula g, f)); (hyps, as_formula g)]

  | ("intro_and", []) ->
     begin
       match f with
       | And (g, h) -> [(hyps, g); (hyps, h)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_and_left", [g]) ->
     [(hyps, And (f, as_formula g))]

  | ("elim_and_right", [g]) ->
     [(hyps, And (as_formula g, f))]

  | ("intro_or_left", []) ->
     begin
       match f with
       | Or (g, _) -> [(hyps, g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("intro_or_right", []) ->
     begin
       match f with
       | Or (_, h) -> [(hyps, h)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_or", [UppIdent h; a; b]) -> (* même ident pour a et b ???? *)
     let (a, b) = (as_formula a, as_formula b) in

     [(hyps, Or (a, b)); ((h, a)::hyps, f); ((h, b)::hyps, f)]

  | ("intro_not", [UppIdent h]) ->
     begin
       match f with
       | Not g -> [((h, g)::hyps, False)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_not", [g]) ->
     begin
       match f with
       | False -> [(hyps, Not (as_formula g)); (hyps, as_formula g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("intro_forall", [Ident x]) ->
     if StringSet.mem x (free_vars f)
        || List.exists (fun (_, h) -> StringSet.mem x (free_vars h)) hyps
     then tactic_error "invalid variable"
     else
       begin
         match f with
         | Forall (y, g) -> [(hyps, rename y x g)]
         | _ -> tactic_error "wrong connector"
       end

  | ("elim_forall", [a; Ident x; t]) ->
     let a = as_formula a in
     let t = as_term t in
     if equal glob f (substitute x t a)
     then [(hyps, Forall (x, a))]
     else tactic_error "wrong argument"

  | ("intro_exists",  [t]) ->
     begin
       match f with
       | Exists (x, g) -> [(hyps, substitute x (as_term t) g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_exists", [g; Ident x; UppIdent h]) ->
     if StringSet.mem x (free_vars f)
        || List.exists (fun (_, h) -> StringSet.mem x (free_vars h)) hyps
     then tactic_error "invalid variable"
     else
       let g = as_formula g in

       [(hyps, Exists (x, g)); ((h, g)::hyps, f)]

  | ("intro_equal", []) ->
     begin
       match f with
       | Atomic (Equal (t1, t2)) when t1 = t2 -> []
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_equal", [a; Ident x; t; u]) ->
     let a = as_formula a in
     let t = as_term t in
     let u = as_term u in
     if equal glob f (substitute x u a)
     then [(hyps, Atomic (Equal (t, u))); (hyps, substitute x t a)]
     else tactic_error "TODO"

  | ("exact", [UppIdent h]) ->
     let hyp = match List.assoc_opt h hyps with
       | Some hyp -> hyp
       | None -> match Context.get_assertion h glob with
                 | Some a -> a
                 | None -> tactic_error "no such hypothesis" in

     if equal glob f hyp then
       []
     else
       tactic_error "hypothesis does not match the goal"
  | _ -> tactic_error "no such tactic"
