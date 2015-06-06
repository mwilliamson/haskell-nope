module Nope.CousCous.Interpreter (run, InterpreterState(interpreterStateStdout)) where

import Data.List (intercalate)
import Control.Monad.State (State, modify, get, put, execState)
import Control.Monad.Except (ExceptT, throwError, catchError, runExceptT)
import qualified Data.Map.Strict as Map

import qualified Nope.CousCous.Nodes as Nodes
import qualified Nope.CousCous.Values as Values
import qualified Nope.CousCous.Interpreter.Heap as Heap

type Variables = Map.Map Nodes.VariableDeclaration Heap.Address
data StackFrame = StackFrame {
    stackFrameStatements :: [Nodes.Statement],
    stackFrameVariables :: Variables
}
data InterpreterState = InterpreterState {
    interpreterStateStdout :: String,
    interpreterStateStack :: [StackFrame],
    interpreterStateHeap :: Heap.Heap
}
type InterpreterStateM = ExceptT String (State InterpreterState)


lookupHeapAddress :: Nodes.VariableDeclaration -> InterpreterState -> Maybe Heap.Address
lookupHeapAddress declaration state =
    let frame = head (interpreterStateStack state)
    in Map.lookup declaration (stackFrameVariables frame)

initialState :: InterpreterState
initialState =
    let emptyState = InterpreterState {
            interpreterStateStdout = "",
            interpreterStateStack = [StackFrame [] Map.empty],
            interpreterStateHeap = Heap.empty
        }
        state' = interpreterStateDeclare (Nodes.Builtin "print") (Just Values.Print) emptyState
        state'' = interpreterStateDeclare (Nodes.Builtin "bool") (Just Values.Bool) state'
    in state''

run :: Nodes.Module -> InterpreterState
run moduleNode =
    let program = runExceptT (execModule moduleNode)
    in execState program initialState


execModule :: Nodes.Module -> InterpreterStateM ()
execModule moduleNode = (do
    pushStackFrameForModule moduleNode
    returnValue <- execStackFrame
    case returnValue of
        Just _ -> throwError "return outside of function"
        Nothing -> return ()
    return ()
    ) `catchError` \exception -> write ("Exception: " ++ exception)

pushStackFrameForModule :: Nodes.Module -> InterpreterStateM ()
pushStackFrameForModule (Nodes.Module declarations statements) =
    pushStackFrame declarations statements

pushStackFrame :: [Nodes.VariableDeclaration] -> [Nodes.Statement] -> InterpreterStateM ()
pushStackFrame declarations statements = modify $ \state ->
    let stack = interpreterStateStack state
        frame = (head stack) { stackFrameStatements = statements }
        state' = state {interpreterStateStack = frame:stack }
    in foldr (\declaration -> interpreterStateDeclare declaration Nothing) state' declarations



execStackFrame :: InterpreterStateM (Maybe Values.Value)
execStackFrame = (do
    state <- get
    let frame:frames = interpreterStateStack state
    case stackFrameStatements frame of
        [] -> popStackFrame >>= \_ -> return Nothing
        statement:statements -> do
            let frame' = frame { stackFrameStatements = statements }
            put $ state { interpreterStateStack = frame':frames }
            returnValue <- exec statement
            case returnValue of
                Nothing -> execStackFrame
                Just _ -> do { popStackFrame; return returnValue }
    ) `catchError` \exception -> do { popStackFrame; throwError exception }

popStackFrame :: InterpreterStateM ()
popStackFrame = modify $ \state ->
    state {interpreterStateStack = tail (interpreterStateStack state) }

exec :: Nodes.Statement -> InterpreterStateM (Maybe Values.Value)

exec (Nodes.ExpressionStatement expression) = do
    _ <- eval expression
    return Nothing

exec (Nodes.Assign (Nodes.VariableReference declaration) valueExpression) = do
    value <- eval valueExpression
    assign declaration value
    return Nothing

exec (Nodes.Assign func _) =
    raise ("cannot assign to " ++ (describeExpressionType func))

exec (Nodes.If conditionExpression trueBranch falseBranch) = do
    conditionValue <- eval conditionExpression
    case conditionValue of
        Values.BooleanValue True -> execAll trueBranch
        Values.BooleanValue False -> execAll falseBranch
        _ -> raise "condition must be bool"
    return Nothing

exec (Nodes.FunctionDefinition declaration scope statements) = do
    let (Nodes.VariableDeclaration name _) = declaration
    assign declaration (Values.Function name scope statements)
    return Nothing

exec (Nodes.Return expression) = do
    value <- eval expression
    return (Just value)


assign :: Nodes.VariableDeclaration -> Values.Value -> InterpreterStateM ()
assign declaration value = do
    state <- get
    heapAddress <- lookupHeapAddressOrException declaration
    let heap' = Heap.assign heapAddress value (interpreterStateHeap state)
    put $ state { interpreterStateHeap = heap' }

interpreterStateDeclare :: Nodes.VariableDeclaration -> Maybe Values.Value -> InterpreterState -> InterpreterState
interpreterStateDeclare declaration value state = 
    let frame:frames = interpreterStateStack state
        (heapAddress, heap') = Heap.declare value (interpreterStateHeap state)
        variables' = Map.insert declaration heapAddress (stackFrameVariables frame)
        frame' = frame { stackFrameVariables = variables' }
    in state {
        interpreterStateStack = frame':frames,
        interpreterStateHeap = heap'
    }

execAll :: [Nodes.Statement] -> InterpreterStateM ()
execAll statements = (mapM exec statements) >>= (const (return ()))

eval :: Nodes.Expression -> InterpreterStateM Values.Value
eval Nodes.NoneLiteral = return Values.None
eval (Nodes.IntegerLiteral value) = return (Values.IntegerValue value)
eval (Nodes.BooleanLiteral value) = return (Values.BooleanValue value)
eval (Nodes.Call func args) = do
    funcValue <- eval func
    argValues <- evalAll args
    call funcValue argValues
eval (Nodes.VariableReference declaration) = do
    heapAddress <- lookupHeapAddressOrException declaration
    state <- get
    let value = Heap.lookup heapAddress (interpreterStateHeap state)
    valueOrException value ("unbound variable: '" ++ (Nodes.variableDeclarationName declaration) ++ "'")


lookupHeapAddressOrException :: Nodes.VariableDeclaration -> InterpreterStateM Heap.Address
lookupHeapAddressOrException declaration = do
    state <- get
    valueOrException (lookupHeapAddress declaration state) ("undefined variable: '" ++ (Nodes.variableDeclarationName declaration) ++ "'")

valueOrException :: Maybe a -> String -> InterpreterStateM a
valueOrException (Just value) _ = return value
valueOrException Nothing exception = raise exception

call :: Values.Value -> [Values.Value] -> InterpreterStateM Values.Value

call (Values.Function _ declarations statements) [] = do
    pushStackFrame declarations statements
    returnValue <- execStackFrame
    case returnValue of
        Nothing -> return Values.None
        Just value -> return value
    

call Values.Print values = do
    write ((intercalate " " (map Values.str values)) ++ "\n")
    return Values.None

call Values.Bool [Values.None] = return Values.false
call Values.Bool [Values.BooleanValue False] = return Values.false
call Values.Bool [Values.IntegerValue 0] = return Values.false
call Values.Bool [_] = return Values.true

call func _ = raise ((Values.str func) ++ " is not callable")

raise :: String -> InterpreterStateM a
raise = throwError
    

write :: [Char] -> InterpreterStateM ()
write value = modify $ \state ->
    state {interpreterStateStdout = (interpreterStateStdout state) ++ value}

evalAll :: [Nodes.Expression] -> InterpreterStateM [Values.Value]
evalAll = mapM eval

describeExpressionType :: Nodes.Expression -> String
describeExpressionType (Nodes.Call _ _) = "function call"
describeExpressionType Nodes.NoneLiteral = "None"
describeExpressionType (Nodes.BooleanLiteral _) = "boolean literal"
describeExpressionType (Nodes.IntegerLiteral _) = "integer literal"
describeExpressionType (Nodes.VariableReference _) = "variable reference"
