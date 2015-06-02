module Nope.Desugaring where

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous
import Nope.NameResolution

desugar :: ResolvedModule -> CousCous.Module
desugar nopeModule = CousCous.Module $ concat (map desugarStatement (Nope.statements nopeModule))

desugarStatement :: ResolvedStatement -> [CousCous.Statement]
desugarStatement (Nope.ExpressionStatement expression) =
    [CousCous.ExpressionStatement $ desugarExpression expression]
desugarStatement (Nope.Assign targets value) =
    let cousCousValue = desugarExpression value
        cousCousTargets = map desugarExpression targets
        tmpReference = CousCous.VariableReference (CousCous.Temporary 1)
    in case cousCousTargets of
        [cousCousTarget] -> [CousCous.Assign cousCousTarget cousCousValue]
        _ ->
            let tmpAssignment = CousCous.Assign tmpReference cousCousValue
                targetAssignments = map (\cousCousTarget -> CousCous.Assign cousCousTarget tmpReference) cousCousTargets
            in tmpAssignment : targetAssignments

desugarExpression :: ResolvedExpression -> CousCous.Expression
desugarExpression (Nope.Call func args) =
    CousCous.Call (desugarExpression func) (map desugarExpression args)
desugarExpression (Nope.VariableReference declaration) =
    CousCous.VariableReference (desugarVariableDeclaration declaration)
desugarExpression (Nope.Literal literal) = desugarLiteral literal


desugarLiteral :: Nope.Literal -> CousCous.Expression
desugarLiteral Nope.NoneLiteral = CousCous.NoneLiteral
desugarLiteral (Nope.IntegerLiteral value) = CousCous.Literal value

desugarVariableDeclaration :: VariableDeclaration -> CousCous.VariableDeclaration
desugarVariableDeclaration (VariableDeclaration name declarationId) =
    CousCous.VariableDeclaration name declarationId
desugarVariableDeclaration (Builtin name) =
    CousCous.Builtin name
