module Nope.CousCous.Interpreter where

import Data.List (intercalate)
import Control.Monad.State (State, modify, get, execState)
import Control.Monad.Except (ExceptT, throwError, catchError, runExceptT)
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Values as Values

data Environment = Environment { stdout :: String, variables :: Variables}
type Variables = Map.Map Nodes.VariableDeclaration Values.Value
type InterpreterState = ExceptT EarlyExit (State Environment)

data EarlyExit = RaisedException String | ReturnValue Values.Value

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
    handleEscapedException (execAll statements)


handleEscapedException :: InterpreterState () -> InterpreterState ()
handleEscapedException state = state `catchError` \earlyExit -> case earlyExit of
        (RaisedException message) -> write ("Exception: " ++ message)
        _ -> state

exec :: Nodes.Statement -> InterpreterState ()

exec (Nodes.ExpressionStatement expression) = do
    _ <- eval expression
    return ()

exec (Nodes.Assign (Nodes.VariableReference declaration) valueExpression) = do
    value <- eval valueExpression
    assign declaration value

exec (Nodes.Assign func _) =
    raise ("cannot assign to " ++ (describeExpressionType func))

exec (Nodes.If conditionExpression trueBranch falseBranch) = do
    conditionValue <- eval conditionExpression
    case conditionValue of
        Values.BooleanValue True -> execAll trueBranch
        Values.BooleanValue False -> execAll falseBranch
        _ -> raise "condition must be bool"

exec (Nodes.FunctionDefinition declaration statements) =
    let (Nodes.VariableDeclaration name _) = declaration
    in assign declaration (Values.Function name statements)

exec (Nodes.Return expression) =
    eval expression >>= (throwError . ReturnValue)


assign :: Nodes.VariableDeclaration -> Values.Value -> InterpreterState ()
assign declaration value = modify $ \state -> 
    let variables' = Map.insert declaration value (variables state)
    in state {variables = variables'}

execAll :: [Nodes.Statement] -> InterpreterState ()
execAll statements = (mapM exec statements) >>= (const (return ()))

eval :: Nodes.Expression -> InterpreterState Values.Value
eval Nodes.NoneLiteral = return Values.None
eval (Nodes.IntegerLiteral value) = return (Values.IntegerValue value)
eval (Nodes.BooleanLiteral value) = return (Values.BooleanValue value)
eval (Nodes.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues
eval (Nodes.VariableReference declaration) = do
    state <- get
    case Map.lookup declaration (variables state) of
        (Just value) -> return $ value
        Nothing -> raise ("undefined variable: '" ++ (Nodes.variableDeclarationName declaration) ++ "'")

call :: Values.Value -> [Values.Value] -> InterpreterState Values.Value

call (Values.Function _ statements) [] =
    consumeReturnValue (execAll statements)
    

call Values.Print values = do
    write ((intercalate " " (map Values.str values)) ++ "\n")
    return Values.None

call Values.Bool [Values.None] = return Values.false
call Values.Bool [Values.BooleanValue False] = return Values.false
call Values.Bool [Values.IntegerValue 0] = return Values.false
call Values.Bool [_] = return Values.true

call func _ = raise ((Values.str func) ++ " is not callable")

raise :: String -> InterpreterState a
raise = throwError . RaisedException

consumeReturnValue :: InterpreterState a -> InterpreterState Values.Value
consumeReturnValue state =
    let noneState = state >>= (const (return Values.None))
    in noneState `catchError` \earlyExit ->
    case earlyExit of
        (ReturnValue value) -> return $ value
        _ -> noneState
    

write :: [Char] -> InterpreterState ()
write value = modify $ \state ->
    state {stdout = (stdout state) ++ value}

evalAll :: [Nodes.Expression] -> InterpreterState [Values.Value]
evalAll = mapM eval

describeExpressionType :: Nodes.Expression -> String
describeExpressionType (Nodes.Call _ _) = "function call"
describeExpressionType Nodes.NoneLiteral = "None"
describeExpressionType (Nodes.BooleanLiteral _) = "boolean literal"
describeExpressionType (Nodes.IntegerLiteral _) = "integer literal"
describeExpressionType (Nodes.VariableReference _) = "variable reference"
