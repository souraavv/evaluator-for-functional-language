import Ast
import Evaluator
import HashMap
import Lexer
import Parser
import System.Environment
import Typing

-- Refer to Readme.md for the make file commands.

main = do
  get <- getArgs
  let testFile = head (get :: [String])
  contents <- readFile testFile
  let input = contents :: String
  let tokens = scanTokens input
  putStrLn "Lexer Ouput: "
  print tokens
  let ast = parser tokens
  putStrLn "Parser Ouput: "
  print ast
  print $ typeOf (Mp []) ast
  let finalAns = eval ast (Mp [])
  putStr "Actual Output ::  "
  print finalAns
