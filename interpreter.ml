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
open Tactics

type global_context = (ident * formula) list

type goal = (ident * formula) list * formula

(* Not cool at all *)

type context' = Main of global_context
              | WaitProof of global_context * (ident * formula)
              | InProof of global_context * (ident * formula) * goal list

type context = Init
             | Running of language * context'


exception ContextError

let context_error () = raise ContextError

let interpret' ctx cmd =
  match ctx with
  | Main glob ->
     begin
       match cmd with
       | Axiom (id, f) -> Main ((id, f)::glob)
       | Theorem (id, f) -> WaitProof (glob, (id, f))
       | _ -> context_error ()
     end
  | WaitProof (glob, (id, f)) ->
     begin
       match cmd with
         Proof -> InProof (glob, (id, f), [ ([], f) ])
       | _ -> context_error ()
     end
  | InProof (glob, f, []) ->
     if cmd = Qed then
       Main (f::glob)
     else
       context_error ()
  | InProof (glob, f, goal :: tl) ->
     begin
       match cmd with
       | Tactic t -> InProof (glob, f, interpret_tactic glob goal t @ tl)
       | Qed -> failwith "Unfinished proof"
       | _ -> context_error ()
     end

let interpret ctx cmd =
  match ctx with
  | Init ->
     begin
       match cmd with
       | Language (cl, fl, rl) -> Running (lang_of_descr (cl, fl, rl), Main [])
       | _ -> context_error ()
     end
  | Running (l, ctx') ->
     Running (l, interpret' ctx' cmd)
