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

type lexical_context = (ident * formula_def) list

type assertion_context = (ident * formula) list

type t = lexical_context * assertion_context

let empty = ([], [])

let add_assertion id f (lc, ac) =
  (lc, (id, f) :: ac)

let get_assertion id (_, ac) =
  List.assoc_opt id ac

let add_def id f (lc, ac) =
  ((id, f) :: lc, ac)

let get_def id (lc, _) =
  List.assoc_opt id lc
