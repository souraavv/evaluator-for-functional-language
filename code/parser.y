{
module Parser where
import Ast
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
       ";"     { EOF $$ }
       "("     { LPAREN $$ }
       ")"     { RPAREN $$ }
       -- int ops
       "+"     { PLUS $$ }
       "-"     { MINUS $$ }
       "*"     { TIMES $$ }
       "~"     { NEGATE $$ }
       "="     { EQUALS $$ }
       "<"     { LESSTHAN $$ }
       ">"     { GREATERTHAN $$ }
       -- bool ops
       "!"     { NOT $$ }
       "&&"    { AND $$ }
       "||"    { OR $$ }
       "^"     { XOR $$ }
       "=>"    { IMPLIES $$ }
       -- ite
       "if"    { IF $$ }
       "then"  { THEN $$ }
       "else"  { ELSE $$ }
       "fi"    { FI $$ }
       -- let
       "let"   { LET $$ }
       ":="    { ASSIGN $$ }
       "in"    { IN $$ }
       "end"   { END $$ }
       -- atoms
       "false" { FALSE $$ }
       "true"  { TRUE $$ }
       int     { CONST $$ }
       var     { ID $$ }
       -- functions
       "fun"  {FUN $$}
       "fn"     {FN $$}
       "->"  {TYPEDEF $$}
       "::" {TYPESIG $$}
       "int" {TYPEINT $$}
       "bool" {TYPEBOOL $$}

%right "->" "=>"
%left ">" "<" "="
%left "+" "-"
%left "*"
%left "^" "||" "&&"
%right "!" "~"

%%

Start
    : Expr ";"				                 { $1 }

Decl
    : var ":=" Expr                          {Decl $1 $3 }

Type 
    : Type "->" Type                         {ARROW $1 $3}
    | "int"                                  {INT}
    | "bool"                                 {BOOL}

Expr
    : "(" Expr ")"                           { $2 }

    -- int ops
    | "~" Expr                               { UnExpr Negate $2 }
    | Expr "+" Expr                          { BinExpr Plus $1 $3 }
    | Expr "-" Expr                          { BinExpr Minus $1 $3 }
    | Expr "*" Expr                          { BinExpr Times $1 $3 }
    | Expr "=" Expr                          { BinExpr Equals $1 $3 }
    | Expr "<" Expr                          { BinExpr LessThan $1 $3 }
    | Expr ">" Expr                          { BinExpr GreaterThan $1 $3 }

    -- bool ops
    | "!" Expr                               { UnExpr Not $2 }
    | Expr "&&" Expr                         { BinExpr And $1 $3 }
    | Expr "||" Expr                         { BinExpr Or $1 $3 }
    | Expr "^" Expr                          { BinExpr Xor $1 $3 }
    -- | Expr "=>" Expr                      { BinExpr Implies $1 $3 }

    -- specials
    | "let" Decl "in" Expr "end"             { Let $2 $4 }
    | "if" Expr "then" Expr "else" Expr "fi" { Ite $2 $4 $6 }

    -- atom
    | "false"                                { BoolConst False }
    | "true"                                 { BoolConst True }
    | int                                    { IntConst $1 }
    | var                                    { VarExpr $1 }


    -- functions

    | "(" Expr Expr ")"                                             {AppExp $2 $3}
    | "fun" var "(" var "::" Type ")" "::" Type  "=>" Expr "end"    {FunExp $2 $4 $6 $9 $11}
    | "fn" "(" var  "::" Type  ")"  "=>" Expr "end"                 {Fn $3 $5 $8}
    


{

parseError [] = error $ "Syntax Error in the last token"
parseError (x:xs) = error $ "Syntax Error: " ++ (show x)

}
