module Nope.CousCous.Interpreter where

import Control.Monad.State

import qualified Nope.CousCous as CC
import Nope



data InterpreterState = InterpreterState Stdout

initialState = InterpreterState (Stdout "")


run :: CC.StatementNode -> Result
run statement =
    let (InterpreterState stdout) = execState (exec statement) initialState in
    Result stdout

exec :: CC.StatementNode -> State InterpreterState ()
exec (CC.ExpressionStatement expression) =
    eval expression >>= \_ -> return ()


eval :: CC.ExpressionNode -> State InterpreterState CC.Value
eval (CC.Literal value) = return (CC.IntegerValue value)
eval (CC.Builtin "print") = return CC.Print
eval (CC.Call func args) =
    eval func >>= \funcValue -> evalAll args >>= \argValues -> call funcValue argValues

call :: CC.Value -> [CC.Value] -> State InterpreterState CC.Value
call CC.Print [CC.IntegerValue value] =
    write ((show value) ++ "\n") >>= \_ -> return CC.Unit

write :: [Char] -> State InterpreterState ()
write value =
    get >>= \(InterpreterState (Stdout stdout)) -> return (InterpreterState (Stdout (stdout ++ value))) >>= put

evalAll :: [CC.ExpressionNode] -> State InterpreterState [CC.Value]
evalAll [] = return []
evalAll (e:es) =
    eval e >>= \v -> evalAll es >>= \vs -> return (v:vs)
