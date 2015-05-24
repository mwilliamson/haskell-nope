module Nope.CousCous.Interpreter where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Values as Values

data InterpreterState = InterpreterState Stdout Variables
data Stdout = Stdout String deriving (Show, Eq)
data Variables = Variables (Map.Map String Values.Value)

initialState :: InterpreterState
initialState = InterpreterState (Stdout "") (Variables (Map.singleton "print" Values.Print))

stdout :: InterpreterState -> String
stdout (InterpreterState (Stdout output) _) = output

run :: Nodes.Module -> InterpreterState
run (Nodes.Module statements) = execState (mapM exec statements) initialState

exec :: Nodes.Statement -> State InterpreterState ()
exec (Nodes.ExpressionStatement expression) = do
    _ <- eval expression
    return ()
exec (Nodes.Assign (Nodes.VariableReference name) valueExpression) = do
    value <- eval valueExpression
    modify $ \(InterpreterState output (Variables variables)) ->
        let variables' = Map.insert name value variables
        in InterpreterState output (Variables variables')
-- TODO: error
exec (Nodes.Assign _ _) = undefined

eval :: Nodes.Expression -> State InterpreterState Values.Value
eval (Nodes.Literal value) = return (Values.IntegerValue value)
eval (Nodes.Builtin "print") = return Values.Print
-- TODO: error
eval (Nodes.Builtin _) = undefined
eval (Nodes.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues
eval (Nodes.VariableReference name) = do
    (InterpreterState _ (Variables variables)) <- get
    -- TODO: handle errors
    let (Just value) = Map.lookup name variables
    return $ value

call :: Values.Value -> [Values.Value] -> State InterpreterState Values.Value
call Values.Print [Values.IntegerValue value] = do
    write ((show value) ++ "\n")
    return Values.None
    -- TODO: error
call _ _ = undefined

write :: [Char] -> State InterpreterState ()
write value = modify $ \(InterpreterState (Stdout output) variables) ->
    (InterpreterState (Stdout (output ++ value)) variables)

evalAll :: [Nodes.Expression] -> State InterpreterState [Values.Value]
evalAll = mapM eval
