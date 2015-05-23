module Nope.CousCous.Interpreter where

import Control.Monad.State

import qualified Nope.CousCous as CC

data InterpreterState = InterpreterState Stdout
data Stdout = Stdout String

initialState :: InterpreterState
initialState = InterpreterState (Stdout "")

stdout :: InterpreterState -> String
stdout (InterpreterState (Stdout output)) = output

run :: CC.ModuleNode -> InterpreterState
run (CC.ModuleNode statements) = execState (mapM exec statements) initialState

exec :: CC.StatementNode -> State InterpreterState ()
exec (CC.ExpressionStatement expression) = do
    _ <- eval expression
    return ()

eval :: CC.ExpressionNode -> State InterpreterState CC.Value
eval (CC.Literal value) = return (CC.IntegerValue value)
eval (CC.Builtin "print") = return CC.Print
-- TODO: error
eval (CC.Builtin _) = undefined
eval (CC.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues

call :: CC.Value -> [CC.Value] -> State InterpreterState CC.Value
call CC.Print [CC.IntegerValue value] = do
    write ((show value) ++ "\n")
    return CC.Unit
    -- TODO: error
call _ _ = undefined

write :: [Char] -> State InterpreterState ()
write value = modify $ \(InterpreterState (Stdout output)) ->
    (InterpreterState (Stdout (output ++ value)))

evalAll :: [CC.ExpressionNode] -> State InterpreterState [CC.Value]
evalAll = mapM eval
