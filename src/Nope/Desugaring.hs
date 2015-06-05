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
    statements <- mapM desugarStatement (Nope.statements nopeModule)
    temporaryDeclarations <- popStackFrame
    let declarations = (declarationsInModule nopeModule) ++ temporaryDeclarations
    return $ CousCous.Module declarations (concat statements)


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
    map desugarVariableDeclaration moduleScope

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
    CousCous.VariableReference (desugarVariableDeclaration declaration)
desugarExpression (Nope.Literal literal) = desugarLiteral literal


desugarLiteral :: Nope.Literal -> CousCous.Expression
desugarLiteral Nope.NoneLiteral = CousCous.NoneLiteral
desugarLiteral (Nope.IntegerLiteral value) = CousCous.IntegerLiteral value

desugarVariableDeclaration :: VariableDeclaration -> CousCous.VariableDeclaration
desugarVariableDeclaration (VariableDeclaration name declarationId) =
    CousCous.VariableDeclaration name declarationId
desugarVariableDeclaration (Builtin name) =
    CousCous.Builtin name
