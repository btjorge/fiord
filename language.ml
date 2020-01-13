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

type name = string

(* There will be more types later (for functions ?) *)
type s_type = Prefix of int (* arity *)
            | Infix         (* arity is 2 *)

type symbol_type = Constant
                 | Function of s_type
                 | Relation of s_type

type language = name list * (name * s_type) list * (name * s_type) list

let empty = ([], [], [])

let get_type (cl, fl, rl) name =
  if List.mem name cl then
    Some Constant
  else
    match List.assoc_opt name fl with
    | Some t -> Some (Function  t)
    | None -> match List.assoc_opt name rl with
              | Some t -> Some (Relation t)
              | None -> None

let lang_of_descr (cl, fl, rl) : language =
  (cl, fl, rl)
