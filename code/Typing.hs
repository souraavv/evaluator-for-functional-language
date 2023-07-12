module Typing where

import Ast
import HashMap
import Lexer
import Parser

-- For typing we will pass the AST.

allBinDomainKeys = [Plus, Minus, Times, And, Or, Xor, GreaterThan, LessThan, Equals]

allBinDomainValues = [[INT], [INT], [INT], [BOOL], [BOOL], [BOOL], [INT], [INT], [INT, BOOL]]

allBinRangeValues = [INT, INT, BOOL, BOOL, BOOL, BOOL, BOOL, BOOL]

allUnDomainKeys = [Negate, Not]

allUnDomainValues = [[INT], [BOOL]]

allUnRangeValues = [INT, BOOL]

makeMap (Mp k) [] [] = Mp k
makeMap (Mp k) (x : xs) (y : ys) = makeMap (Mp ((x, y) : k)) xs ys

domainOfBin = makeMap (Mp []) allBinDomainKeys allBinDomainValues

rangeOfBin = makeMap (Mp []) allBinDomainKeys allBinRangeValues

domainOfUn = makeMap (Mp []) allUnDomainKeys allUnDomainValues

rangeOfUn = makeMap (Mp []) allUnDomainKeys allUnRangeValues

typeOf symbolTable (IntConst x) = INT
typeOf symbolTable (BoolConst x) = BOOL
typeOf symbolTable (VarExpr x) = if isPresent symbolTable x then giveVal symbolTable x else error $ "Type Error :: Variable " ++ show x ++ " Not Declared "
{-type checking for the Binary Expressoin.-}

typeOf symbolTable (BinExpr operator exp1 exp2)
  | type1 == type2 = if isValidDomain && isValidRange then giveVal rangeOfBin operator else error "Type error"
  | otherwise = error $ "Type Error :: Mis-matched Type of Operands for the operator " ++ show operator
  where
    type1 = typeOf symbolTable exp1
    type2 = typeOf symbolTable exp2
    isValidDomain = isPresent domainOfBin operator
    oprDomain = giveVal domainOfBin operator
    isValidRange = check oprDomain type1

{-Type checking for Unary Expression-}

typeOf symbolTable (UnExpr operator exp1)
  | isValidDomain && isValidRange = giveVal rangeOfUn operator
  | otherwise = error $ "Type Error :: Type not matching with the operator :: " ++ show operator
  where
    type1 = typeOf symbolTable exp1
    isValidDomain = isPresent domainOfUn operator
    oprDomain = giveVal domainOfUn operator
    isValidRange = check oprDomain type1

{-Type checking for the If then else fi expr;-}

typeOf symbolTable (Ite cond expTrue expFalse)
  | isCondTypeBool = if bothExprSameType then typeTrueExp else error $ "Type Error :: Mismatch type in then and else " ++ show typeTrueExp ++ " != " ++ show typeFalseExp
  | otherwise = error $ "Type Error: Needed Boolean condition in the If expression instead of " ++ show type1
  where
    type1 = typeOf symbolTable cond
    isCondTypeBool = type1 == BOOL
    typeTrueExp = typeOf symbolTable expTrue
    typeFalseExp = typeOf symbolTable expFalse
    bothExprSameType = typeTrueExp == typeFalseExp

{-Type checking for Let expr-}

typeOf symbolTable (Let (Decl var expr1) expr2) = typeExpr2
  where
    type1 = typeOf symbolTable expr1
    newSymbolTable = addVal symbolTable var type1
    typeExpr2 = typeOf newSymbolTable expr2

{-
Type checking for application : given exp1 type must need to match with the input type of function.
                                Function are simple structure they take single input and thus
                                we need to abstract the first of arrow only
-}

typeOf symbolTable (AppExp f inputExpr) = case f of
  (VarExpr funcName) ->
    if isPresent symbolTable funcName
      then
        ( let (ARROW a b) = giveVal symbolTable funcName
              inputType = typeOf symbolTable inputExpr
           in if a == inputType
                then b
                else error $ "Type Error : Argument to function type not matched " ++ "Expected Type" ++ show a ++ " with Actual Type : " ++ show inputType ++ " in function name : " ++ show funcName
        )
      else error $ "Type Error :: Not a function name " ++ show funcName
  (Fn var inputType exp1) ->
    let newTable = addVal symbolTable var inputType
        givenInputType = typeOf symbolTable inputExpr
     in if inputType == givenInputType
          then typeOf newTable exp1
          else error $ "Type Error : Argument to function type not matched " ++ "Expected Type" ++ show inputType ++ " with Actual Type : " ++ show givenInputType
  (FunExp funName var inputType outputType body) ->
    let newTable = addVal symbolTable var inputType
        newTable1 = addVal newTable funName (ARROW inputType outputType)
        givenInputType = typeOf symbolTable inputExpr
     in if inputType == givenInputType
          then
            let retType = typeOf newTable1 body
             in if outputType == retType
                  then retType
                  else error $ "Type Error:: Return Type mis-matched Expected Type: " ++ show outputType ++ " with Actual Type : " ++ show retType
          else error $ "Type Error : Argument to function type not matched " ++ "Expected Type" ++ show inputType ++ " with Actual Type : " ++ show givenInputType
  (AppExp fun2 arg2) ->
    let (ARROW funInputType funOutputType) = typeOf symbolTable fun2 --symbolTable (AppExp fun2 arg2)
        givenInputType = typeOf symbolTable arg2 -- typeOf symbolTable inputExpr
     in if funInputType == givenInputType
          then funOutputType
          else error $ "Type Error : Argument to function type not matched " ++ "Expected Type" ++ show funInputType ++ " with Actual Type : " ++ show givenInputType
  _ -> error "Type Error : Expected function name "
-- ARROW

{-Type checking for function:-}
typeOf symbolTable (FunExp funName var inputType outputType body)
  | bodyRetType == outputType = ARROW inputType outputType
  | otherwise = error $ "Type Error:: Return type not matching " ++ "Expected Type : " ++ show outputType ++ " with Actual Type : " ++ show bodyRetType
  where
    symbolTable1 = addVal symbolTable var inputType
    symbolTable2 = addVal symbolTable1 funName (ARROW inputType outputType)
    bodyRetType = typeOf symbolTable2 body

{-
Type checking for Anonymous function : its return type in a function which take varType ->
-}

typeOf symbolTable (Fn var inputType exp1) = ARROW inputType outputType
  where
    newSymbolTable = addVal symbolTable var inputType
    outputType = typeOf newSymbolTable exp1

check :: (Foldable t, Eq a) => t a -> a -> Bool
check xs val = foldr (\x -> (||) (x == val)) False xs