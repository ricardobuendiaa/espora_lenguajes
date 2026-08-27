{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }

-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;
--   * operadores estrictamente binarios: expt y eq;
--   * operadores unarios: not, add1, sub1, zero?.

--   * operadores n-arios con al menos dos argumentos;
    | '(' '+' narios ')'   { Add $3 } 
    | '(' '-' narios ')'   { Sub $3 } 
    | '(' '*' narios ')'   { Mul $3 } 
    | '(' '/' narios ')'   { Div $3 } 
    | '(' "and" narios ')' { And $3 }
    | '(' "or" narios ')'  { Or $3 }
    | '(' '<' narios ')'   { Lt $3 }
    | '(' '>' narios ')'   { Gt $3 }
    | '(' "<=" narios ')'  { Le $3 }
    | '(' ">=" narios ')'  { Ge $3 }

--   * operadores estrictamente binarios: expt y eq;

    | '(' "expt" ASA ASA ')' { Expt $3 $4 }
    | '(' "eq" ASA ASA ')'   { EqP $3 $4 }

--   * operadores unarios: not, add1, sub1, zero?.

    | '(' "not" ASA ')'    { Not $3 }
    | '(' "add1" ASA ')'   { Add1 $3 }
    | '(' "sub1" ASA ')'   { Sub1 $3 }
    | '(' "zero?" ASA ')'  { ZeroP $3 }


-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.

    narios : ASA  narios { $1 : $2 }
           | ASA  ASA      { [$1, $2] }

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}
