module Nope.Desugaring (desugar) where

import Control.Monad.State (State, get, put, modify, evalState)

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous
import Nope.NameResolution

data DesugarState = DesugarState {
    desugarStateTemporaryIndex :: Int,
    desugarStateTemporaryStack:: [[CousCous.VariableDeclaration]]
}
type DesugarStateM = State DesugarState

desugar :: ResolvedModule -> CousCous.Module
desugar nopeModule =
    evalState (desugarModule nopeModule) (DesugarState 1 [])

desugarModule :: ResolvedModule -> DesugarStateM CousCous.Module
desugarModule nopeModule = do
    pushStackFrame []
    statements <- desugarStatements (Nope.statements nopeModule)
    temporaryDeclarations <- popStackFrame
    let declarations = (declarationsInModule nopeModule) ++ temporaryDeclarations
    return $ CousCous.Module declarations statements


pushStackFrame :: [CousCous.VariableDeclaration] -> DesugarStateM ()
pushStackFrame frame = modify $ \state ->
    state { desugarStateTemporaryStack = frame:(desugarStateTemporaryStack state) }

popStackFrame :: DesugarStateM [CousCous.VariableDeclaration]
popStackFrame = do
    state <- get
    let stack = desugarStateTemporaryStack state
    put state { desugarStateTemporaryStack = tail stack }
    return $ head stack

declareVariable :: CousCous.VariableDeclaration -> DesugarStateM ()
declareVariable declaration = do
    declarations <- popStackFrame
    pushStackFrame (declaration:declarations)
    

declarationsInModule :: ResolvedModule -> [CousCous.VariableDeclaration]
declarationsInModule Nope.Module { Nope.moduleScope = moduleScope } =
    map desugarDeclaration moduleScope


desugarStatements :: [ResolvedStatement] -> DesugarStateM [CousCous.Statement]
desugarStatements statements = do
    desugarredStatements <- mapM desugarStatement statements
    return $ concat desugarredStatements


desugarStatement :: ResolvedStatement -> DesugarStateM [CousCous.Statement]

desugarStatement (Nope.ExpressionStatement expression) =
    return $ [CousCous.ExpressionStatement $ desugarExpression expression]

desugarStatement (Nope.Assign targets value) = do
    let cousCousValue = desugarExpression value
    let cousCousTargets = map desugarExpression targets
    tmpReference <- createTemporary
    return $ case cousCousTargets of
        [cousCousTarget] -> [CousCous.Assign cousCousTarget cousCousValue]
        _ ->
            let tmpAssignment = CousCous.Assign tmpReference cousCousValue
                targetAssignments = map (\cousCousTarget -> CousCous.Assign cousCousTarget tmpReference) cousCousTargets
            in tmpAssignment : targetAssignments

desugarStatement function@Nope.Function{} = do
    body <- desugarStatements (Nope.functionBody function)
    return [CousCous.Function {
        CousCous.functionDeclaration = desugarDeclaration (Nope.functionTarget function),
        CousCous.functionArguments = [],
        CousCous.functionLocalDeclarations = [],
        CousCous.functionBody = body
    }]

desugarStatement (Nope.Return value) = do
    let value' = desugarExpression value
    return [(CousCous.Return value')]


createTemporary :: DesugarStateM CousCous.Expression
createTemporary = do
    index <- increment
    let temporary = CousCous.Temporary index
    declareVariable temporary
    return $ CousCous.VariableReference temporary

increment :: DesugarStateM Int
increment = do
    state <- get
    let index = desugarStateTemporaryIndex state
    put state { desugarStateTemporaryIndex = index + 1 }
    return index

desugarExpression :: ResolvedExpression -> CousCous.Expression
desugarExpression (Nope.Call func args) =
    CousCous.Call (desugarExpression func) (map desugarExpression args)
desugarExpression (Nope.VariableReference declaration) =
    CousCous.VariableReference (desugarDeclaration declaration)
desugarExpression (Nope.Literal literal) = desugarLiteral literal


desugarLiteral :: Nope.Literal -> CousCous.Expression
desugarLiteral Nope.NoneLiteral = CousCous.NoneLiteral
desugarLiteral (Nope.IntegerLiteral value) = CousCous.IntegerLiteral value

desugarDeclaration :: VariableDeclaration -> CousCous.VariableDeclaration
desugarDeclaration (VariableDeclaration name declarationId) =
    CousCous.VariableDeclaration name declarationId
desugarDeclaration (Builtin name) =
    CousCous.Builtin name
