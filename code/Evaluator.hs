module Evaluator where 
import Ast
import HashMap
import Lexer
import Parser
import Typing

giveInt (IntConst val) = val

giveBool (BoolConst val) = val

giveFn (Fn arg argType body) = Fn arg argType body

eval (IntConst val) symbolTable = IntConst val
eval (BoolConst val) symbolTable = BoolConst val
-- Binary Expressoin

eval (VarExpr val) symbolTable = if isPresent symbolTable val then giveVal symbolTable val else error $ "Evaluation Error :: Variable " ++ show val ++ " Not In Scope! "
eval (UnExpr operator e1) symbolTable = case operator of
  Negate -> IntConst (negate (giveInt retVal))
  Not -> BoolConst (not (giveBool retVal))
  where
    retVal = eval e1 symbolTable

-- Binary Expression

eval (BinExpr operator e1 e2) symbolTable = case operator of
  Plus -> IntConst (a + b)
  Minus -> IntConst (a - b)
  Times -> IntConst (a * b)
  LessThan -> BoolConst (a < b)
  GreaterThan -> BoolConst (a > b)
  And -> BoolConst (c && d)
  Xor -> BoolConst (c /= d)
  Or -> BoolConst (c || d)
  Equals -> case retVal1 of
    (IntConst _) -> BoolConst (a == b)
    (BoolConst _) -> BoolConst (c == d)
  where
    retVal1 = eval e1 symbolTable
    retVal2 = eval e2 symbolTable
    a = giveInt retVal1
    b = giveInt retVal2
    c = giveBool retVal1
    d = giveBool retVal2

-- eval of Let expression
eval (Let (Decl var exp) inExpr) symbolTable = eval inExpr newTable
  where
    newTable = addVal symbolTable var (eval exp symbolTable)

-- eval of Ite Expression

eval (Ite cond expTrue expFalse) symbolTable = if checkTrue then eval expTrue symbolTable else eval expFalse symbolTable
  where
    checkTrue = giveBool (eval cond symbolTable)

-- eval of anon function

eval (Fn arg argType body) symbolTable = Fn arg argType body
-- eval of named-function

eval (FunExp funName var inputType outputType body) symbolTable = FunExp funName var inputType outputType body
-- evaluation in case of (application)

-- funName could look like =
-- (Fn arg1 argType body) arg
-- (FunExp funName var inputType outputType body) arg
--
eval (AppExp funName arg) symbolTable = case funName of
  (VarExpr funName2) ->
    if isPresent symbolTable funName2
      then case giveVal symbolTable funName2 of
        (Fn arg1 arg1Type body) ->
          let newTable = addVal symbolTable arg1 valArg
           in eval body newTable
        (FunExp funName var inputType outputType body) ->
          let newTable = addVal (addVal symbolTable var valArg) funName (FunExp funName var inputType outputType body)
           in eval body newTable
      else error $ "Function is not available or Out of Scope " ++ show funName2
  (AppExp funName3 arg2) -> case eval (AppExp funName3 arg2) symbolTable of
    (Fn arg1 arg1Type body) ->
      let newTable = addVal symbolTable arg1 valArg
       in eval body newTable
    (FunExp funName var inputType outputType body) ->
      let newTable = addVal (addVal symbolTable var valArg) funName (FunExp funName var inputType outputType body)
       in eval body newTable
    otherwise -> error "Error: Needed a valid function to evaluate "
  (Fn arg1 arg1Type body) -> let newTable = addVal symbolTable arg1 valArg in eval body newTable
  (FunExp funName var inputType outputType body) ->
    let newTable = addVal (addVal symbolTable var valArg) funName (FunExp funName var inputType outputType body)
     in eval body newTable
  where
    valArg = eval arg symbolTable
