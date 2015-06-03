module Nope.CousCous.Interpreter where

import Data.List (intercalate)
import Control.Monad.State (State, modify, get, execState)
import Control.Monad.Except (ExceptT, throwError, catchError, runExceptT)
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Values as Values

data Environment = Environment { stdout :: String, variables :: Variables}
type Variables = Map.Map Nodes.VariableDeclaration Values.Value
type InterpreterState = ExceptT String (State Environment)

initialState :: Environment
initialState = Environment {
    stdout = "",
    variables = Map.fromList [
        ((Nodes.Builtin "print"), Values.Print),
        ((Nodes.Builtin "bool"), Values.Bool)
    ]
}

run :: Nodes.Module -> Environment
run moduleNode =
    let program = runExceptT (execModule moduleNode)
    in execState program initialState


execModule :: Nodes.Module -> InterpreterState ()
execModule (Nodes.Module statements) =
    ((mapM exec statements) >>= (const (return ()))) `catchError`
        \message -> write ("Exception: " ++ message)

exec :: Nodes.Statement -> InterpreterState ()
exec (Nodes.ExpressionStatement expression) = do
    _ <- eval expression
    return ()
exec (Nodes.Assign (Nodes.VariableReference declaration) valueExpression) = do
    value <- eval valueExpression
    modify $ \state -> 
        let variables' = Map.insert declaration value (variables state)
        in state {variables = variables'}
exec (Nodes.Assign func _) = throwError ("cannot assign to " ++ (describeExpressionType func))

eval :: Nodes.Expression -> InterpreterState Values.Value
eval Nodes.NoneLiteral = return Values.None
eval (Nodes.Literal value) = return (Values.IntegerValue value)
eval (Nodes.BooleanLiteral value) = return (Values.BooleanValue value)
eval (Nodes.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues
eval (Nodes.VariableReference declaration) = do
    state <- get
    case Map.lookup declaration (variables state) of
        (Just value) -> return $ value
        Nothing -> throwError ("undefined variable: '" ++ (Nodes.variableDeclarationName declaration) ++ "'")

call :: Values.Value -> [Values.Value] -> InterpreterState Values.Value
call Values.Print values = do
    write ((intercalate " " (map Values.str values)) ++ "\n")
    return Values.None
call Values.Bool [Values.None] = return Values.false
call Values.Bool [Values.BooleanValue False] = return Values.false
call Values.Bool [Values.IntegerValue 0] = return Values.false
call Values.Bool [_] = return Values.true
call func _ = throwError ((Values.str func) ++ " is not callable")

write :: [Char] -> InterpreterState ()
write value = modify $ \state ->
    state {stdout = (stdout state) ++ value}

evalAll :: [Nodes.Expression] -> InterpreterState [Values.Value]
evalAll = mapM eval

describeExpressionType :: Nodes.Expression -> String
describeExpressionType (Nodes.Call _ _) = "function call"
describeExpressionType Nodes.NoneLiteral = "None"
describeExpressionType (Nodes.BooleanLiteral _) = "boolean literal"
describeExpressionType (Nodes.Literal _) = "integer literal"
describeExpressionType (Nodes.VariableReference _) = "variable reference"
