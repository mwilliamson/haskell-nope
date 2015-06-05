module Nope.Desugaring (desugar) where

import Control.Monad.State (State, get, put, evalState)

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous
import Nope.NameResolution

type Counter = State Int

desugar :: ResolvedModule -> CousCous.Module
desugar nopeModule =
    evalState (desugarModule nopeModule) 1

desugarModule :: ResolvedModule -> Counter CousCous.Module
desugarModule nopeModule = do
    statements <- mapM desugarStatement (Nope.statements nopeModule)
    counter <- get
    let temporaryDeclarations = map CousCous.Temporary (enumFromTo 1 counter)
        declarations = (declarationsInModule nopeModule) ++ temporaryDeclarations
    return $ CousCous.Module declarations (concat statements)


declarationsInModule :: ResolvedModule -> [CousCous.VariableDeclaration]
declarationsInModule Nope.Module { Nope.moduleScope = moduleScope } =
    map desugarVariableDeclaration moduleScope

desugarStatement :: ResolvedStatement -> Counter [CousCous.Statement]
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


createTemporary :: Counter CousCous.Expression
createTemporary = do
    index <- increment
    return $ CousCous.VariableReference (CousCous.Temporary index)

increment :: Counter Int
increment = do
    index <- get
    put (index + 1)
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
