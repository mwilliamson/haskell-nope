module Nope.Desugaring where

import qualified Nope.Nodes as Nope
import qualified Nope.CousCous.Nodes as CousCous

desugar :: Nope.Module -> CousCous.Module
desugar (Nope.Module statements) = CousCous.Module $ map desugarStatement statements

desugarStatement :: Nope.Statement -> CousCous.Statement
desugarStatement (Nope.ExpressionStatement expression) =
    CousCous.ExpressionStatement $ desugarExpression expression
desugarStatement (Nope.Assign [target] value) =
    CousCous.Assign (desugarExpression target) (desugarExpression value)
desugarStatement (Nope.Assign _ _) =
    undefined

desugarExpression :: Nope.Expression -> CousCous.Expression
desugarExpression Nope.NoneLiteral = CousCous.NoneLiteral
desugarExpression (Nope.Literal value) = CousCous.Literal value
desugarExpression (Nope.Call func args) =
    CousCous.Call (desugarExpression func) (map desugarExpression args)
desugarExpression (Nope.Builtin name) = CousCous.Builtin name
desugarExpression (Nope.VariableReference name) = CousCous.VariableReference name
