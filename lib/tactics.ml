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

let get_hypothesis glob hyps h =
  match List.assoc_opt h hyps with
  | Some hyp -> hyp
  | None ->
     match Context.get_assertion h glob with
     | Some hyp -> hyp
     | None -> tactic_error "no such hypothesis"

(* Inspecter correctement cette fonction. *)
let interpret_tactic glob (hyps, goal) = function
  | ("intro_arrow", [UppIdent h]) ->
     begin
       match goal with
       | Arrow (f, g) -> [((h, f) :: hyps, g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_arrow", [f]) ->
     [(hyps, Arrow (as_formula f, goal)); (hyps, as_formula f)]

  | ("intro_and", []) ->
     begin
       match goal with
       | And (f, g) -> [(hyps, f); (hyps, g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_and_left", [f]) ->
     [(hyps, And (goal, as_formula f))]

  | ("elim_and_right", [f]) ->
     [(hyps, And (as_formula f, goal))]

  | ("intro_or_left", []) ->
     begin
       match goal with
       | Or (f, _) -> [(hyps, f)]
       | _ -> tactic_error "wrong connector"
     end

  | ("intro_or_right", []) ->
     begin
       match goal with
       | Or (_, g) -> [(hyps, g)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_or", [UppIdent h; a; b]) -> (* même ident pour a et b ???? *)
     let (a, b) = (as_formula a, as_formula b) in

     [(hyps, Or (a, b)); ((h, a)::hyps, goal); ((h, b)::hyps, goal)]

  | ("intro_not", [UppIdent h]) ->
     begin
       match goal with
       | Not g -> [((h, g)::hyps, False)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_not", [f]) ->
     begin
       match goal with
       | False -> [(hyps, Not (as_formula f)); (hyps, as_formula f)]
       | _ -> tactic_error "wrong connector"
     end

  | ("intro_forall", [Ident x]) ->
     if StringSet.mem x (free_vars goal)
        || List.exists (fun (_, h) -> StringSet.mem x (free_vars h)) hyps
     then tactic_error "invalid variable"
     else
       begin
         match goal with
         | Forall (y, f) -> [(hyps, rename y x f)]
         | _ -> tactic_error "wrong connector"
       end

  | ("elim_forall", [a; Ident x; t]) ->
     let a = as_formula a in
     let t = as_term t in
     if equal glob goal (substitute x t a)
     then [(hyps, Forall (x, a))]
     else tactic_error "wrong argument"

  | ("intro_exists",  [t]) ->
     begin
       match goal with
       | Exists (x, f) -> [(hyps, substitute x (as_term t) f)]
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_exists", [f; Ident x; UppIdent h]) ->
     if StringSet.mem x (free_vars goal)
        || List.exists (fun (_, h) -> StringSet.mem x (free_vars h)) hyps
     then tactic_error "invalid variable"
     else
       let f = as_formula f in

       [(hyps, Exists (x, f)); ((h, f)::hyps, goal)]

  | ("intro_equal", []) ->
     begin
       match goal with
       | Atomic (Equal (t1, t2)) when t1 = t2 -> []
       | _ -> tactic_error "wrong connector"
     end

  | ("elim_equal", [a; Ident x; t; u]) ->
     let a = as_formula a in
     let t = as_term t in
     let u = as_term u in
     if equal glob goal (substitute x u a)
     then [(hyps, Atomic (Equal (t, u))); (hyps, substitute x t a)]
     else tactic_error "TODO"

  | ("exact", [UppIdent h]) ->
     let hyp = get_hypothesis glob hyps h in

     if equal glob goal hyp then
       []
     else
       tactic_error "hypothesis does not match the goal"

  | ("apply", [UppIdent h]) ->

     let rec concls hyps f =
       (f, hyps) :: match f with
                    | And (g, h) -> concls hyps g @ concls hyps h
                    | Arrow (g, h) -> concls (g::hyps) h
                    | _ -> [] in

     let hyp = get_hypothesis glob hyps h in

     begin
       match List.assoc_opt goal (concls [] hyp) with
       | None -> tactic_error "not a consequence"
       | Some hys -> List.map (fun hy -> (hyps, hy)) hys
     end

  | ("specialize", [UppIdent h; UppIdent h1; t]) ->

     let t = as_term t in
     let hyp = get_hypothesis glob hyps h in

     begin
       match hyp with
       | Forall (x, g) -> [((h1, substitute x t g) :: hyps, goal)]
       | _ -> tactic_error "expect an forall hypothesis"
     end

  | _ -> tactic_error "no such tactic"
