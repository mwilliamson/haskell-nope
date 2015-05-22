module Nope.Parsing where

import qualified Language.Python.Common.AST as Python
import Language.Python.Version3.Parser
import Language.Python.Common.ParseError

import qualified Nope.Nodes as Nodes

parse :: String -> Either ParseError Nodes.Module
parse input = do
    (moduleSpan, comments) <- parseModule input ""
    return $ transformModule moduleSpan


transformModule :: Python.ModuleSpan -> Nodes.Module
transformModule (Python.Module statements) =
    Nodes.Module $ map transformStatement statements


transformStatement :: Python.StatementSpan -> Nodes.Statement
transformStatement (Python.StmtExpr expression _) =
    Nodes.ExpressionStatement $ transformExpression expression


transformExpression :: Python.ExprSpan -> Nodes.Expression
transformExpression (Python.Int value _ _) = Nodes.Literal value
transformExpression (Python.Call func args _) =
    Nodes.Call (transformExpression func) (map transformArgument args)
transformExpression (Python.Var (Python.Ident name _) _) = Nodes.Builtin name


transformArgument :: Python.ArgumentSpan -> Nodes.Expression
transformArgument (Python.ArgExpr expression _) = transformExpression expression
