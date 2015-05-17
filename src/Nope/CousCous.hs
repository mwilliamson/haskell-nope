module Nope.CousCous where

expression_statement :: ExpressionNode -> StatementNode
expression_statement = undefined

call :: ExpressionNode -> [ExpressionNode] -> ExpressionNode
call = undefined

builtin :: [Char] -> ExpressionNode
builtin = undefined

literal :: Integer -> ExpressionNode
literal = undefined

data ExpressionNode = ExpressionNode

data StatementNode = StatementNode
