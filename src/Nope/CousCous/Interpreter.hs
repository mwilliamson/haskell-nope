module Nope.CousCous.Interpreter where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous as CC

data InterpreterState = InterpreterState Stdout Variables
data Stdout = Stdout String deriving (Show, Eq)
data Variables = Variables (Map.Map String CC.Value)

initialState :: InterpreterState
initialState = InterpreterState (Stdout "") (Variables (Map.singleton "print" CC.Print))

stdout :: InterpreterState -> String
stdout (InterpreterState (Stdout output) _) = output

run :: CC.ModuleNode -> InterpreterState
run (CC.ModuleNode statements) = execState (mapM exec statements) initialState

exec :: CC.StatementNode -> State InterpreterState ()
exec (CC.ExpressionStatement expression) = do
    _ <- eval expression
    return ()
exec (CC.Assign (CC.VariableReference name) valueExpression) = do
    value <- eval valueExpression
    modify $ \(InterpreterState output (Variables variables)) ->
        let variables' = Map.insert name value variables
        in InterpreterState output (Variables variables')
-- TODO: error
exec (CC.Assign _ _) = undefined

eval :: CC.ExpressionNode -> State InterpreterState CC.Value
eval (CC.Literal value) = return (CC.IntegerValue value)
eval (CC.Builtin "print") = return CC.Print
-- TODO: error
eval (CC.Builtin _) = undefined
eval (CC.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues
eval (CC.VariableReference name) = do
    (InterpreterState _ (Variables variables)) <- get
    -- TODO: handle errors
    let (Just value) = Map.lookup name variables
    return $ value

call :: CC.Value -> [CC.Value] -> State InterpreterState CC.Value
call CC.Print [CC.IntegerValue value] = do
    write ((show value) ++ "\n")
    return CC.Unit
    -- TODO: error
call _ _ = undefined

write :: [Char] -> State InterpreterState ()
write value = modify $ \(InterpreterState (Stdout output) variables) ->
    (InterpreterState (Stdout (output ++ value)) variables)

evalAll :: [CC.ExpressionNode] -> State InterpreterState [CC.Value]
evalAll = mapM eval
