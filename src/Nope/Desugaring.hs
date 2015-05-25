module Nope.Desugaring where

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous
import Nope.Parsing (ParsedModule, ParsedStatement, ParsedExpression)

desugar :: ParsedModule -> CousCous.Module
desugar nopeModule = CousCous.Module $ concat (map desugarStatement (Nope.statements nopeModule))

desugarStatement :: ParsedStatement -> [CousCous.Statement]
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

desugarExpression :: ParsedExpression -> CousCous.Expression
desugarExpression Nope.NoneLiteral = CousCous.NoneLiteral
desugarExpression (Nope.Literal value) = CousCous.Literal value
desugarExpression (Nope.Call func args) =
    CousCous.Call (desugarExpression func) (map desugarExpression args)
desugarExpression (Nope.Builtin name) = CousCous.Builtin name
desugarExpression (Nope.VariableReference name) = CousCous.VariableReference name
