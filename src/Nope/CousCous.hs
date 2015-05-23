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
    VariableReference String |
    Literal Integer |
    Builtin [Char] |
    Call ExpressionNode [ExpressionNode]
    
data ModuleNode = ModuleNode [StatementNode]    

data StatementNode =
    ExpressionStatement ExpressionNode |
    Assign ExpressionNode ExpressionNode

data Value = Unit | IntegerValue Integer | Print
