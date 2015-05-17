module Nope.CousCous.Interpreter where

import qualified Nope.CousCous as CC
import Nope


data State = State Stdout

initialState = State (Stdout "")


run :: CC.StatementNode -> Result
run statement =
    let (State stdout) = exec statement initialState in
    Result stdout

exec :: CC.StatementNode -> State -> State
exec (CC.ExpressionStatement expression) state =
    let (_, state2) = eval expression state in
    state2


eval :: CC.ExpressionNode -> State -> (CC.Value, State)
eval (CC.Literal value) state = (CC.IntegerValue value, state)
eval (CC.Builtin "print") state = (CC.Print, state)
eval (CC.Call func args) state =
    let (funcValue, state2) = eval func state
        (argValues, state3) = evalAll args state2
    in call funcValue argValues state3

call :: CC.Value -> [CC.Value] -> State -> (CC.Value, State)
call CC.Print [CC.IntegerValue value] (State (Stdout stdout)) =
    (CC.Unit, State (Stdout (stdout ++ (show value) ++ "\n")))


evalAll :: [CC.ExpressionNode] -> State -> ([CC.Value], State)
evalAll [] state = ([], state)
evalAll (e:es) state =
    let (v, state2) = eval e state
        (vs, state3) = evalAll es state2
    in (v:vs, state3)
