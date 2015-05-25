module Nope.CousCous.Interpreter where

import Data.List (intercalate)
import Control.Monad.State (State, modify, get, execState)
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Values as Values

data Environment = Environment { stdout :: String, variables :: Variables}
type Variables = Map.Map String Values.Value
type InterpreterState = State Environment

initialState :: Environment
initialState = Environment {stdout = "", variables = Map.singleton "print" Values.Print}

run :: Nodes.Module -> Environment
run (Nodes.Module statements) = execState (mapM exec statements) initialState

exec :: Nodes.Statement -> InterpreterState ()
exec (Nodes.ExpressionStatement expression) = do
    _ <- eval expression
    return ()
exec (Nodes.Assign (Nodes.VariableReference name) valueExpression) = do
    value <- eval valueExpression
    modify $ \state -> 
        let variables' = Map.insert name value (variables state)
        in state {variables = variables'}
-- TODO: error
exec (Nodes.Assign _ _) = undefined

eval :: Nodes.Expression -> InterpreterState Values.Value
eval Nodes.NoneLiteral = return Values.None
eval (Nodes.Literal value) = return (Values.IntegerValue value)
eval (Nodes.Builtin "print") = return Values.Print
-- TODO: error
eval (Nodes.Builtin _) = undefined
eval (Nodes.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues
eval (Nodes.VariableReference name) = do
    state <- get
    -- TODO: handle errors
    let (Just value) = Map.lookup name (variables state)
    return $ value

call :: Values.Value -> [Values.Value] -> InterpreterState Values.Value
call Values.Print values = do
    write ((intercalate " " (map Values.str values)) ++ "\n")
    return Values.None
    -- TODO: error
call _ _ = undefined

write :: [Char] -> InterpreterState ()
write value = modify $ \state ->
    state {stdout = (stdout state) ++ value}

evalAll :: [Nodes.Expression] -> InterpreterState [Values.Value]
evalAll = mapM eval
