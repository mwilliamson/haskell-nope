module Nope.CousCous where

expression_statement :: ExpressionNode -> StatementNode
expression_statement expression = ExpressionStatement expression

call :: ExpressionNode -> [ExpressionNode] -> ExpressionNode
call func args = Call func args

builtin :: [Char] -> ExpressionNode
builtin name = Builtin name

literal :: Integer -> ExpressionNode
literal value = Literal value

data ExpressionNode =
    Literal Integer |
    Builtin [Char] |
    Call ExpressionNode [ExpressionNode]
    
    

data StatementNode = ExpressionStatement ExpressionNode

data Value = Unit | IntegerValue Integer | Print
