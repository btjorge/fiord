/******************************************************************************/
/*  Copyright 2019 Benjamin Terra-Jorge                                       */
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
%token LANGUAGE AXIOM THEOREM PROOF QED
%token <string> CONST FUNC REL FVAR
%token <string> IDENT
%token <int> NUMBER

%nonassoc COMMA /* pour les quantificateurs */
%right ARROW
%left AND
%left OR
%nonassoc NOT

%start <Ast.command> sentence

%%

/*program:
| prog = list (sentence) EOF  { prog }*/

sentence:
| c = command DOT  { c }

command: /* nom ??? */
| LANGUAGE COLON RBRACE consts = separated_list (COMMA, IDENT) LBRACE
                 RBRACE funcs = separated_list (COMMA, decl) LBRACE
                 RBRACE rels = separated_list (COMMA, decl) LBRACE
  { Language (consts, funcs, rels) }
| AXIOM id = IDENT DEFINED f = formula  { Axiom (id, f) }
| THEOREM id = IDENT DEFINED f = formula  { Theorem(id, f) }
| PROOF  { Proof }
| QED    { Qed }
| id = IDENT args = list (argument)   { Tactic (id, args) }

decl:
| id = IDENT COLON num = NUMBER  { (id, num) }

argument:
| id = IDENT  { Ident id }
| fv = FVAR  { Formula (FVar fv) }
| LPAREN id = IDENT RPAREN  { Ident id }
| LPAREN t = not_id_term RPAREN  { Term t }
| LPAREN f = formula RPAREN { Formula f }

formula:
| FALSE  { False }
| a = atomic  { Atomic a }
| fv = FVAR { FVar fv }
| LPAREN f = formula RPAREN  { f }
| NOT f = formula  { Not f }
| f = formula AND g = formula  { And (f, g) }
| f = formula OR g = formula  { Or (f, g) }
| f = formula ARROW g = formula  { Arrow (f, g) }
| FORALL vars = list (IDENT) COMMA f = formula  { Forall (vars, f) }
| EXISTS vars = list (IDENT) COMMA f = formula  { Exists (vars, f) }

atomic:
| t1 = term EQUAL t2 = term  { Equal (t1, t2) }
| r = REL args = nonempty_list(atomic_term)  { Rel (r, args) }

term:
| st = atomic_term  { st }
| func = FUNC args = nonempty_list (atomic_term)  { Func (func, args) }

not_id_term:
| LPAREN t = term RPAREN  { t }
| func = FUNC args = nonempty_list (atomic_term)  { Func (func, args) }

atomic_term: /* nom nul ! */
| c = CONST  { Const c }
| id = IDENT  { Var id }
| LPAREN t = term RPAREN  { t }


