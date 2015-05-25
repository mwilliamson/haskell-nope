module Nope.Desugaring where

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous

desugar :: (Nope.Module String) -> CousCous.Module
desugar (Nope.Module statements) = CousCous.Module $ concat (map desugarStatement statements)

desugarStatement :: (Nope.Statement String) -> [CousCous.Statement]
desugarStatement (Nope.ExpressionStatement expression) =
    [CousCous.ExpressionStatement $ desugarExpression expression]
desugarStatement (Nope.Assign targets value) =
    let cousCousValue = desugarExpression value
        cousCousTargets = map desugarExpression targets
        tmpReference = CousCous.VariableReference "tmp"
    in case cousCousTargets of
        [cousCousTarget] -> [CousCous.Assign cousCousTarget cousCousValue]
        _ ->
            let tmpAssignment = CousCous.Assign tmpReference cousCousValue
                targetAssignments = map (\cousCousTarget -> CousCous.Assign cousCousTarget tmpReference) cousCousTargets
            in tmpAssignment : targetAssignments

desugarExpression :: (Nope.Expression String) -> CousCous.Expression
desugarExpression Nope.NoneLiteral = CousCous.NoneLiteral
desugarExpression (Nope.Literal value) = CousCous.Literal value
desugarExpression (Nope.Call func args) =
    CousCous.Call (desugarExpression func) (map desugarExpression args)
desugarExpression (Nope.Builtin name) = CousCous.Builtin name
desugarExpression (Nope.VariableReference name) = CousCous.VariableReference name
