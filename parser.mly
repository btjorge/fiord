/******************************************************************************/
/*  Copyright 2019-2020 Benjamin Terra-Jorge                                  */
/*                                                                            */
/*  Licensed under the Apache License, Version 2.0 (the "License");           */
/*  you may not use this file except in compliance with the License.          */
/*  You may obtain a copy of the License at                                   */
/*                                                                            */
/*      http://www.apache.org/licenses/LICENSE-2.0                            */
/*                                                                            */
/*  Unless required by applicable law or agreed to in writing, software       */
/*  distributed under the License is distributed on an "AS IS" BASIS,         */
/*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  */
/*  See the License for the specific language governing permissions and       */
/*  limitations under the License.                                            */
/*                                                                            */
/******************************************************************************/

%{
    open Ast
    open Language
%}

/*%token EOF*/
%token EQUAL
%token NOT AND OR ARROW /* IMPLIES ? */
%token EXISTS FORALL
%token FALSE
%token COMMA
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COLON
%token DOT
%token DEFINED
%token INFIX
%token LANGUAGE AXIOM THEOREM PROOF QED
%token <string> BINOP BINREL
%token <string> CONST FUNC REL
%token <string> IDENT
%token <string> UPPIDENT
%token <string> NUMBER /* oui, c'est un peu bizarre */
%token <string> SYMBOL

%nonassoc COMMA /* pour les quantificateurs */
%right ARROW
%left AND
%left OR
%nonassoc NOT

%left BINOP

%start <Ast.command> sentence

%%

/*program:
| prog = list (sentence) EOF  { prog }*/

sentence:
| c = command DOT  { c }

command: /* nom ??? */
| LANGUAGE COLON LBRACE consts = separated_list (COMMA, symbol) RBRACE
                 LBRACE funcs = separated_list (COMMA, decl) RBRACE
                 LBRACE rels = separated_list (COMMA, decl) RBRACE
  { Language (consts, funcs, rels) }
| AXIOM id = UPPIDENT DEFINED f = formula  { Axiom (id, f) }
| THEOREM id = UPPIDENT DEFINED f = formula  { Theorem (id, f) }
| PROOF  { Proof }
| QED    { Qed }
| id = IDENT args = list (argument)   { Tactic (id, args) }

%inline symbol:
| id = IDENT  { id }
| id = UPPIDENT  { id }
| n = NUMBER  { n }
| s = SYMBOL  { s }

(* pour l'instant *)
decl:
| symb = symbol COLON num = NUMBER  { (symb, Prefix (int_of_string num)) }
| symb = symbol COLON INFIX       { (symb, Infix) }

argument:
| id = IDENT  { Ident id }
| id = UPPIDENT  { UppIdent id  }
| LPAREN id = IDENT RPAREN  { Ident id }
| LPAREN t = not_id_term RPAREN  { Term t }
| LPAREN f = formula RPAREN { Formula f }

formula:
| FALSE  { False }
| a = atomic  { Atomic a }
| id = UPPIDENT { FVar id }
| LPAREN f = formula RPAREN  { f }
| NOT f = formula  { Not f }
| f = formula AND g = formula  { And (f, g) }
| f = formula OR g = formula  { Or (f, g) }
| f = formula ARROW g = formula  { Arrow (f, g) }
| FORALL vars = list (IDENT) COMMA f = formula
    { List.fold_right (fun v f -> Forall (v, f)) vars f }
| EXISTS vars = list (IDENT) COMMA f = formula
    { List.fold_right (fun v f -> Exists (v, f)) vars f }

atomic:
| t1 = term EQUAL t2 = term  { Equal (t1, t2) }
| t1 = term r = BINREL t2 = term  { Rel (r, [t1; t2]) }
| r = REL args = nonempty_list(atomic_term)  { Rel (r, args) }

term:
| st = atomic_term  { st }
| t1 = term op = BINOP t2 = term  { Func (op, [t1; t2]) }
| func = FUNC args = nonempty_list (atomic_term)  { Func (func, args) }

not_id_term:
| LPAREN t = term RPAREN  { t }
| c = CONST  { Const c }
| t1 = term op = BINOP t2 = term  { Func (op, [t1; t2]) }
| func = FUNC args = nonempty_list (atomic_term)  { Func (func, args) }

atomic_term: /* nom nul ! */
| c = CONST  { Const c }
| id = IDENT  { Var id }
| LPAREN t = term RPAREN  { t }
