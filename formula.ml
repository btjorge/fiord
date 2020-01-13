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

let is_digit c =
  '0' <= c && c <= '9'

let split_var v =
  let len = String.length v in
  let i = ref (len - 1) in

  while !i >= 0 && is_digit v.[!i] do
    i := !i - 1
  done;
  i := !i + 1;

  let prefix = String.sub v 0 !i in

  if !i = len
  then (prefix, -1)
  else (prefix, int_of_string (String.sub v !i (len - !i)))

module StringSet = Set.Make (String)

let fresh_var used base =
  let (prefix, num) = split_var base in
  let used = StringSet.elements used in
  let used = List.map split_var used in
  let used = List.filter (fun (p, _) -> p = prefix) used in
  if not (List.mem (prefix, num) used) then
    base
  else
    let nmax = List.fold_left (fun m (_, n) -> max n m) (-1) used + 1 in
    prefix ^ string_of_int nmax

let rec vars_term = function
  | Const _ -> StringSet.empty
  | Var v -> StringSet.singleton v
  | Func (_, tl) ->
     List.fold_left (fun s t -> StringSet.union s (vars_term t))
                    StringSet.empty tl

let vars_atomic = function
  | Equal (t1, t2) -> StringSet.union (vars_term t1) (vars_term t2)
  | Rel (_, tl) ->
     List.fold_left (fun s t -> StringSet.union s (vars_term t))
                    StringSet.empty tl

let rec free_vars = function
  | False | FVar _ -> StringSet.empty
  | Atomic a -> vars_atomic a
  | Not f -> free_vars f
  | And (f, g) | Or (f, g)
  | Arrow (f, g) -> StringSet.union (free_vars f) (free_vars g)
  | Forall (v, f) | Exists (v, f) -> StringSet.remove v (free_vars f)

let rec vars = function
  | False | FVar _ -> StringSet.empty
  | Atomic a -> vars_atomic a
  | Not f -> vars f
  | And (f, g) | Or (f, g)
  | Arrow (f, g) -> StringSet.union (vars f) (vars g)
  | Forall (v, f) | Exists (v, f) -> StringSet.union (StringSet.singleton v)
                                                     (vars f)

(* Y a sans doute plus efficace *)
let closed f = StringSet.is_empty (free_vars f)

let convertible conv v1 v2 =
  List.exists (fun (x1,x2) -> (x1 = v1 && x2 = v2)||(x1 = v2 && x2 = v1)) conv

let rec list_equal_custom equal l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (h1::t1, h2::t2) -> equal h1 h2 && list_equal_custom equal t1 t2
  | _ -> false

let rec equal_term conv t1 t2 =
  match (t1, t2) with
  | (Const c1, Const c2) -> c1 = c2
  | (Var v1, Var v2) -> v1 = v2 || convertible conv v1 v2
  | (Func (f1, tl1), Func (f2, tl2)) ->
     f1 = f2 && list_equal_custom (equal_term conv) tl1 tl2
  | _ -> false

let equal_atomic conv a1 a2 =
  match (a1, a2) with
  | (Equal (t1, t1'), Equal (t2, t2')) ->
     equal_term conv t1 t2 && equal_term conv t1' t2'
  | (Rel (r1, tl1), Rel (r2, tl2)) ->
     r1 = r2 && list_equal_custom (equal_term conv) tl1 tl2
  | _ -> false

let rec equal_formula ctx conv f1 f2 =
  match (f1, f2) with
  | (False, False) -> true
  | (Atomic a1, Atomic a2) -> equal_atomic conv a1 a2
  | (FVar v1, FVar v2) -> v1 = v2
  | (Not g1, Not g2) -> equal_formula ctx conv g1 g2
  | (And (g1, g1'), And (g2, g2')) | (Or (g1, g1'), Or (g2, g2'))
  | (Arrow (g1, g1'), Arrow (g2, g2')) ->
     equal_formula ctx conv g1 g2 && equal_formula ctx conv g1' g2'
  | (Forall (v1, f1), Forall (v2, f2))
  | (Exists (v1, f1), Exists (v2, f2)) ->
     equal_formula ctx ((v1, v2)::conv) f1 f2
  | (FVar v, f) | (f, FVar v) ->
     begin
       match List.assoc_opt v ctx with
       | None -> false
       | Some g -> equal_formula ctx conv f g
     end
  | _ -> false

let equal ctx f g = equal_formula ctx [] f g

let rec rename_term x y = function
  | Const c -> Const c
  | Var v -> if v = x then Var y else Var v
  | Func (id, tl) -> Func (id, List.map (rename_term x y) tl)

let rename_atomic x y = function
  | Equal (t1, t2) -> Equal (rename_term x y t1, rename_term x y t2)
  | Rel (id, tl) -> Rel (id, List.map (rename_term x y) tl)

let rec rename_unsafe x y = function
  | False -> False
  | Atomic a -> Atomic (rename_atomic x y a)
  | FVar v -> FVar v
  | Not f -> Not (rename_unsafe x y f)
  | And (f, g) -> And (rename_unsafe x y f, rename_unsafe x y g)
  | Or (f, g) -> Or (rename_unsafe x y f, rename_unsafe x y g)
  | Arrow (f, g) -> Arrow (rename_unsafe x y f, rename_unsafe x y g)
  | Forall (v, f) -> if v = x then Forall (v, f)
                     else Forall (v, rename_unsafe x y f)
  | Exists (v, f) -> if v = x then Exists (v, f)
                     else Exists (v, rename_unsafe x y f)

module StringMap = Map.Make (String)

let rec substitute_term smap = function
  | Const c -> Const c
  | Var v ->
     begin
       match StringMap.find_opt v smap with
       | None -> Var v
       | Some t -> t
     end
  | Func (id, tl) -> Func (id, List.map (substitute_term smap) tl)

let substitute_atomic smap = function
  | Equal (t1, t2) ->
     Equal (substitute_term smap t1, substitute_term smap t2)
  | Rel (id, tl) -> Rel (id, List.map (substitute_term smap) tl)

let substitute_map smap f =
  let rec aux smap tvs used = function
    | False -> False
    | Atomic a -> Atomic (substitute_atomic smap a)
    | FVar v -> FVar v
    | Not f -> Not (aux smap tvs used f)
    | And (f, g) -> And (aux smap tvs used f, aux smap tvs used g)
    | Or (f, g) -> Or (aux smap tvs used f, aux smap tvs used g)
    | Arrow (f, g) -> Arrow (aux smap tvs used f, aux smap tvs used g)

    | Forall (v, f) ->
       if StringSet.mem v tvs then
         let v' = fresh_var used v in
         let smap = StringMap.add v (Var v') smap in
         let tvs = StringSet.add v' tvs in
         let used = StringSet.add v' used in
         Forall (v', aux smap tvs used f)
       else
         let smap = StringMap.remove v smap in
         Forall (v, aux smap tvs used f)

    | Exists (v, f) ->
       if StringSet.mem v tvs then
         let v' = fresh_var used v in
         let smap = StringMap.add v (Var v') smap in
         let tvs = StringSet.add v' tvs in
         let used = StringSet.add v' used in
         Exists (v', aux smap tvs used f)
       else
         let smap = StringMap.remove v smap in
         Exists (v, aux smap tvs used f) in

  let folder _ t tvs = StringSet.union tvs (vars_term t) in

  let tvs = StringMap.fold folder smap StringSet.empty in
  let fvs = vars f in
  let used = StringSet.union tvs fvs in

  aux smap tvs used f

let substitute_assoc slist f =
  let folder smap (v, t) = StringMap.add v t smap in
  let smap = List.fold_left folder StringMap.empty slist in
  substitute_map smap f

let substitute x t f =
  substitute_map (StringMap.singleton x t) f

let rename x y f =
  substitute x (Var y) f
